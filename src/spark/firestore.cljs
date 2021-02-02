(ns spark.firestore
  (:require
   [clojure.spec.alpha :as s]

   [cljs-bean.core :as cljs-bean]
   [spark.logging :refer [log]]
   [spark.utils :as u]))


(defn doc? [doc]
  (and
   (-> doc :firestore/path string?)
   (-> doc :firestore/id string?)))


(s/def ::doc doc?)
(s/def ::path-element (s/or :map-path-element map?
                            :string-path-element string?))
(s/def ::path (s/or :string string?
                    :vector (s/coll-of ::path-element)
                    :doc ::doc))
(s/def ::opt-path (s/or :nil-path nil?
                        :non-nil-path ::path))


(defonce FIRESTORE (atom nil))

(defn ^js firestore []
  (if-let [firestore @FIRESTORE]
    firestore
    (throw (js/Error. "FIRESTORE atom not initialized!"))))

;;; helpers

(defn ^js FieldValue []
  (if (exists? js/firebase)
    (-> js/firebase.firestore.FieldValue)
    (-> (js/require "firebase-admin") .-firestore .-FieldValue)))

(defn ^js array-remove [elements]
  (-> ^js (FieldValue) .-arrayRemove (apply (clj->js elements))))

(defn ^js array-union [elements]
  (-> ^js (FieldValue) .-arrayUnion (apply (clj->js elements))))

(defn ^js timestamp []
  (-> ^js (FieldValue) .serverTimestamp))

(defn convert-FieldValue-or-nil [v]
  (when-let [k (if (vector? v) (first v) nil)]
    (cond
      (= k :db/array-union)  (array-union (second v))
      (= k :db/array-remove) (array-remove (second v))
      (= k :db/timestamp) (timestamp)
      :else nil)))

(defn inject-FieldValues [data]
  (reduce (fn [data [k v]]
            (if-let [converted-v (convert-FieldValue-or-nil v)]
              (assoc data k converted-v)
              data
              #_(if (map? v)
                (assoc data k (inject-FieldValues v))
                data)))
          data data))

;;; wrap docs to have access to id and path

(defn wrap-doc [^js query-doc-snapshot]
  (let [data (-> query-doc-snapshot .data)]
    (-> (cljs-bean/->clj data)
        (assoc :firestore/id (-> query-doc-snapshot .-id)
               :firestore/path (-> query-doc-snapshot .-ref .-path)
               :firestore/exists? (boolean data)))))

(defn wrap-docs [^js query-snapshot]
  (mapv wrap-doc (-> query-snapshot .-docs)))


(defn doc-id [doc]
  (-> doc :firestore/id))

(defn doc-path [doc]
  (-> doc :firestore/path))

(defn doc-exists? [doc]
  (-> doc :firestore/exists? boolean))

(defn ^js unwrap-doc [doc]
  (-> doc
      (dissoc :firestore/id :firestore/path :firestore/exists?)
      inject-FieldValues
      clj->js))

;;; collection and doc references


(defn as-path [thing]
  (cond
    (string? thing) (-> thing ( .split "/"))
    (doc? thing)    (-> thing doc-path as-path)
    :else           (do (s/assert ::path thing) thing)))


(defn- fs-collection [source path-elem]
  (if (map? path-elem)
    (let [{:keys [id wheres where]} path-elem
          wheres (if where
                   (conj wheres where)
                   wheres)
          collection (-> ^js source (.collection id))]
      (reduce (fn [collection [attr op val]]
                (-> ^js collection (.where attr op val)))
              collection wheres))
    (-> ^js source (.collection path-elem))))


(defn ^js ref [path]
  (log ::ref
       :path path)
  (s/assert ::opt-path path)
  (when path
    (loop [col nil
           doc nil
           path (as-path path)]
      (if (empty? path)
        (if doc doc col)
        (cond

          doc
          (recur (-> ^js doc (fs-collection (first path)))
                 nil
                 (rest path))

          col
          (recur nil
                 (-> ^js col (.doc (first path)))
                 (rest path))

          :else
          (recur (-> (firestore) (fs-collection (first path)))
                 nil
                 (rest path)))))))


;;;

(defn doc> [path]
  (js/Promise.
   (fn [resolve reject]
     (-> (ref path)
         .get
         (.then (fn [^js doc]
                  (resolve (wrap-doc doc)))
                reject)))))

(defn col> [path]
  (js/Promise.
   (fn [resolve reject]
     (-> (ref path)
         .get
         (.then (fn [^js query-snapshot]
                  (resolve (wrap-docs query-snapshot)))
                reject)))))


(defn create-doc>
  "Creates a new document.
  An existing document will be replaced."
  [path data]
  (log ::create-doc>
       :path path
       :data data)
  (s/assert ::path path)
  (let [^js ref (ref path)
        col-ref? (-> ref .-where boolean)]
    (if col-ref?
      (-> ref (.add (unwrap-doc data)))
      (-> ref (.set (unwrap-doc data))))))


(defn save-doc>
  "Saves the document `doc`."
  [doc]
  (s/assert doc? doc)
  (-> doc
      doc-path
      ref
      (.set ^js (unwrap-doc doc))))


(defn update-fields>
  "Updates fields in an existing document."
  [doc-path fields]
  (log ::update-fields>
       :doc-path doc-path
       :fields fields)
  (s/assert ::path doc-path)
  (s/assert map? fields)
  (-> doc-path
      ref
      (.update (unwrap-doc fields))))


(defn update-child-fields> [doc child-path child-id child-changes]
  (let [changes (reduce (fn [m [k v]]
                          (assoc m
                                 (str child-path "." child-id "." (name k))
                                 v))
                        {} child-changes)]
    (update-fields> doc changes)))


(defn delete-doc>
  [doc-or-path]
  (log ::delete-doc
       :doc doc-or-path)
  (-> doc-or-path ref .delete))


(defn load-and-save>
  "Load, update and save/delete.
  Deletes the document if `update-f` returns nil."
  [doc-path update-f]
  (log ::load-and-save>
       :doc-path doc-path
       :update-f update-f)
  (s/assert ::path doc-path)
  (-> (doc> doc-path)
      (.then #(if (doc-exists? %)
                (let [doc (update-f %)]
                  (cond
                    (= doc :db/delete) (delete-doc> %)
                    doc (save-doc> doc)
                    :else (js/Promise.resolve :db/no-op)))
                (let [data (update-f nil)]
                  (if (seq data)
                    (create-doc> doc-path data)
                    (js/Promise.resolve nil)))))))

(ns spark.firestore
  (:require
   [clojure.spec.alpha :as s]
   [goog.object :as gobj]
   [cljs-bean.core :as cljs-bean]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [clojure.string :as str]))


;; https://firebase.google.com/docs/reference/js/firebase.firestore

(defn doc? [doc]
  (and
   (-> doc :firestore/path string?)
   (-> doc :firestore/id string?)))


(s/def ::doc doc?)
(s/def ::path-element (s/or :map-path-element map?
                            :string-path-element string?))
(s/def ::path (s/or :keyword simple-keyword?
                    :string string?
                    :vector (s/coll-of ::path-element)
                    :doc ::doc))
(s/def ::opt-path (s/or :nil-path nil?
                        :non-nil-path ::path))


(defonce FIRESTORE (atom nil))

(defn ^js firestore []
  (if-let [firestore @FIRESTORE]
    firestore
    (throw (js/Error. "FIRESTORE atom not initialized!"))))

;;; * helpers

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
      (= k :db/timestamp)    (timestamp)
      (= k :db/delete)       (-> ^js (FieldValue) .delete)
      :else                  nil)))

(defn inject-FieldValues [data]
  (reduce (fn [data [k v]]
            (if-let [converted-v (convert-FieldValue-or-nil v)]
              (assoc data k converted-v)
              (if (map? v)
                (assoc data k (inject-FieldValues v))
                data)))
          data data))

(defn remove-metadata [data]
  (let [data (dissoc data
                     :firestore/schema
                     :firestore/doc-path
                     :db/ref)]
    (reduce (fn [data [k v]]
              (if (map? v)
                (assoc data k (remove-metadata v))
                data))
            data data)))

(defn encode-field-key [k]
  (cond
    (qualified-keyword? k) (str (namespace k) ":" (name k))
    (simple-keyword? k)    (name k)
    :else                  k))

(defn decode-field-key [s]
  (let [parts (str/split s ":")]
    (if (-> parts count (> 1))
      (keyword (first parts) (->> parts rest (str/join ":")))
      (keyword (first parts)))))

(defn- entity--encode-keys [data]
  (reduce (fn [data [k v]]
            (let [new-k (encode-field-key k)
                  new-v (if (map? v)
                          (entity--encode-keys v)
                          v)]
              ;; (log ::DEBUG
              ;;      :k k :new-k new-k)
              (assoc data new-k new-v)))
          data data))

;; * conform to schema

(defn conform-js-data [^js data schema db-doc-ref]
  (cond

    (nil? data)
    nil

    (= :keyword schema)
    (keyword data)

    (= :string schema)
    (str data)

    (= :int schema)
    (js/parseInt data)

    (or (string? data) (number? data) (boolean? data))
    (js->clj data)

    (instance? (if (exists? js/firebase)
                 (-> js/firebase.firestore.Timestamp)
                 (-> (js/require "firebase-admin") .-firestore .-Timestamp))
               data)
    (-> data .toDate)

    ^boolean (js/Array.isArray data)
    (case (first schema)
      :set
      (into #{} (map (fn [[idx item]]
                       (let [item-schema (if (map? (second schema))
                                           (nth schema 2)
                                           (nth schema 1))]
                         (conform-js-data item item-schema (conj db-doc-ref idx))))
                     (map-indexed vector data)))

      :vector
      (mapv (fn [[idx item]]
              (let [item-schema (if (map? (second schema))
                                  (nth schema 2)
                                  (nth schema 1))]
                (conform-js-data item item-schema (conj db-doc-ref idx))))
            (map-indexed vector data))

      ;; else
      (mapv (fn [[idx item]]
              (conform-js-data item nil (conj db-doc-ref idx)))
            (map-indexed vector data)))

    (vector? schema)
    (case (first schema)

      :map-of
      (reduce (fn [m js-key]
                (let [k           js-key
                      v           (gobj/get data js-key)
                      k-schema    (if (map? (second schema))
                                    (nth schema 2)
                                    (nth schema 1))
                      v-schema    (if (map? (second schema))
                                    (nth schema 3)
                                    (nth schema 2))
                      k-conformed (conform-js-data k k-schema db-doc-ref)]
                  (assoc m
                         k-conformed
                         (conform-js-data v v-schema (conj db-doc-ref
                                                           k-conformed)))))
              {} (js/Object.keys data))

      ;; else -> :map
      (let [new-data {:db/ref db-doc-ref}
            new-data (if-let [doc-schema-id (or (-> schema second :subdoc-schema/id)
                                                (-> schema second :doc-schema/id))]
                       (assoc new-data :firestore/schema doc-schema-id)
                       new-data)]
        (reduce (fn [m js-key]
                  (let [k        (decode-field-key js-key)
                        v        (gobj/get data js-key)
                        v-schema (u/malli-map-field-schema-by-id schema k)
                        v        (conform-js-data v v-schema (conj db-doc-ref k))]
                    (if (nil? v)
                      m
                      (assoc m k v))))
                new-data (js/Object.keys data))))

    :else
    (js->clj data :keywordize-keys true)))

;; * wrap docs to have access to id and path

(defonce DOC_SCHEMAS (atom {}))

(defn reg-doc-schema [col-id doc-schema]
  (swap! DOC_SCHEMAS assoc col-id doc-schema))

(defn conform-doc-data [^js data schema db-doc-ref]
  (when data
    (-> data
        (conform-js-data schema db-doc-ref)
        (assoc :firestore/schema (-> schema second :doc-schema/id))
        )))

(defn doc-schema [col-id]
  (get @DOC_SCHEMAS col-id))

(defn wrap-doc [^js query-doc-snapshot]
  (let [data       (-> query-doc-snapshot .data)
        path       (-> query-doc-snapshot .-ref .-path)
        col-id     (.substring path 0 (.indexOf path "/"))
        doc-schema (doc-schema col-id)
        db-doc-ref [path]]
    (-> data
        (conform-doc-data doc-schema db-doc-ref)
        (assoc :firestore/id (-> query-doc-snapshot .-id)
               :firestore/path path
               :firestore/exists? (boolean data)
               :db/ref path))))

(defn wrap-docs [^js query-snapshot]
  (mapv wrap-doc (-> query-snapshot .-docs)))


(defn doc-id [doc]
  (-> doc :firestore/id))

(defn doc-path [doc]
  (-> doc :firestore/path))

(defn doc-exists? [doc]
  (-> doc :firestore/exists? boolean))

(defn ^js unwrap-doc [doc]
  ;; (log ::unwrap-doc
  ;;      :doc doc)
  (when doc
    (-> doc
        (dissoc :firestore/id
                :firestore/path
                :firestore/exists?
                :firestore/schema
                :db/ref)
        inject-FieldValues
        remove-metadata
        entity--encode-keys
        clj->js)))

;;; collection and doc references


(defn as-path [thing]
  (cond
    (string? thing)  (-> thing ( .split "/"))
    (doc? thing)     (-> thing doc-path as-path)
    (keyword? thing) [(name thing)]
    :else            (do (s/assert ::path thing) thing)))


(defn- fs-collection [source path-elem]
  (if (map? path-elem)
    (let [{:keys [id wheres where]} path-elem
          wheres                    (if where
                                      (conj wheres where)
                                      wheres)
          wheres                    (remove nil? wheres)
          collection                (-> ^js source (.collection id))]
      (reduce (fn [collection [attr op val]]
                (-> ^js collection (.where attr op (clj->js val))))
              collection wheres))
    (-> ^js source (.collection path-elem))))


(defn ^js ref [path]
  ;; (log ::ref
  ;;      :path path)
  (s/assert ::opt-path path)
  (when path
    (loop [col  nil
           doc  nil
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
  ;; (log ::col>
  ;;      :path path)
  (js/Promise.
   (fn [resolve reject]
     (-> (ref path)
         .get
         (.then (fn [^js query-snapshot]
                  (resolve (wrap-docs query-snapshot)))
                reject)))))

(defn cols-names> []
  (u/=> (-> (firestore) .listCollections)
        #(mapv (fn [^js col] (.-id col)) %)))

(defn create-doc>
  "Creates a new document.
  An existing document will be replaced."
  [path data]
  (log ::create-doc>
       :path path
       :data data)
  (s/assert ::path path)
  (let [^js ref  (ref path)
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


(defn- flatten-entity-map
  ([m]
   (flatten-entity-map nil nil m))
  ([doc prefix m]
   (let [m (remove-metadata m)]
     (reduce (fn [doc [k v]]
               (let [k (encode-field-key k)
                     k (if prefix
                         (str prefix "." k)
                         k)]
                 (if (map? v)
                   (flatten-entity-map doc k v)
                   (assoc doc k v))))
             doc m))))

(comment
  (flatten-entity-map {:id     "1"
                       :name   "witek"
                       :skills {"java"    {:id   "java"
                                           :name "Java"}
                                "clojure" {:id   "clojure"
                                           :name "Clojure"}}}))

(defn update-child-fields> [doc child-path child-id child-changes]
  (let [changes (flatten-entity-map {} (str child-path "." child-id) child-changes)]
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
                    doc                (save-doc> doc)
                    :else              (js/Promise.resolve :db/no-op)))
                (let [data (update-f nil)]
                  (cond
                    (= :db/delete data)
                    (delete-doc> doc-path)

                    (seq data)
                    (create-doc> doc-path data)

                    :else
                    (js/Promise.resolve nil)))))))

;; * transactions

(defn get>
  ([path]
   (get> nil path nil))
  ([transaction path]
   (get> transaction path nil))
  ([^js transaction path not-found]
   (log ::get>
        :path        path
        :transaction transaction)
   (let [ref      (ref path)
         col-ref? (-> ref .-where boolean)]
     (u/=> (if transaction
             (.get transaction ref)
             (.get ref))
           (fn [^js result]
             (if col-ref?
               (wrap-docs result)
               (if (-> result .-exists)
                 (wrap-doc result)
                 not-found)))))))

(comment
  (u/=> (get> ["devtest" "dummy-1"]) u/tap>)
  (u/=> (get> "devtest/dummy-1") u/tap>)
  (u/=> (get> ["devtest"]) u/tap>))


(defn- set>--set-doc> [^js transaction tx-data autocreate?]
  ;; (log ::set>--set-doc>
  ;;      :tx-data tx-data)
  (let [path (or (-> tx-data :firestore/path)
                 (-> tx-data :db/ref))
        ref  (ref path)
        data (unwrap-doc tx-data)]
    (u/=> (if autocreate?
            (if transaction
              (u/=> (get> transaction path)
                    (fn [doc]
                      (if doc
                        (.update transaction ref data)
                        (.set transaction ref data (clj->js {:merge true}))     )))
              (-> (.update ref data)
                  (.catch (fn [_err]
                            (.set ref data (clj->js {:merge true}))))))
            (if transaction
              (u/resolve> (.update transaction ref data))
              (.update ref data)))
          (fn [_] tx-data))))

(defn- set>--delete-doc> [^js transaction tx-data]
  (let [path (or (-> tx-data :firestore/path)
                 (-> tx-data :db/ref))
        ref  (ref path)]
    (u/=> (if transaction
            (u/resolve> (.delete transaction ref))
            (.delete ref))
          (fn [_] tx-data))))

(defn subdoc-prefix-from-db-ref [db-ref]
  (->> db-ref
       rest
       (map (fn [path-element]
              (if (keyword? path-element)
                (name path-element)
                (str path-element))))
       (str/join ".")))

(defn set>--delete-subdoc> [^js transaction tx-data db-ref]
  (set>--set-doc> transaction
                  {:firestore/path                    (first db-ref)
                   (subdoc-prefix-from-db-ref db-ref) [:db/delete]}
                  false))

(defn set>--set-subdoc> [^js transaction tx-data db-ref]
  (let [doc-path (first db-ref)
        entity   (flatten-entity-map {:firestore/path doc-path}
                                     (subdoc-prefix-from-db-ref db-ref)
                                     tx-data)]
    (set>--set-doc> transaction entity false)))

(defn set>
  ([tx-data]
   (set> nil tx-data))
  ([^js transaction tx-data]
   (if (sequential? tx-data)
     (u/=> (u/all> (map #(set> transaction %) tx-data))
           (fn [_]
             tx-data))
     (if-not tx-data
       (u/no-op>)
       (let [db-ref  (-> tx-data :db/ref)
             subdoc? (vector? db-ref)]
         (log ::set>
              :tx-data tx-data
              :transaction transaction)
         (if subdoc?
           (if (-> tx-data :db/delete (= true))
             (set>--delete-subdoc> transaction tx-data db-ref)
             (set>--set-subdoc> transaction tx-data db-ref))
           (if (-> tx-data :db/delete (= true))
             (set>--delete-doc> transaction tx-data)
             (set>--set-doc> transaction tx-data true))))))))

(comment
  (set> nil)
  (set> {:firestore/path "devtest/dummy-1" :hello "world"})
  (set> {:firestore/path "devtest/dummy-1" :hello [:db/delete]})
  (u/tap> (set> {:firestore/path "devtest/dummy-1" :db/delete true}))
  (u/tap> (get> "devtest/dummy-1"))

  (u/=> (get> "devtest/dummy-1")
        (fn [doc]
          (js/console.log "LOADED" doc)
          (set> [ {:db/ref   "devtest/dummy-1"
                   :ts       [:db/timestamp]
                   :children {"a" {:name "a"}
                              "b" {:name "b"}}}]))
        (fn [result]
          (js/console.log "RESULT" result))
        u/tap>)

  (u/tap> (set> {:db/ref    ["devtest/dummy-1" :children "b"]
                 :db/delete true}))

  (u/=> (get> "devtest/dummy-1")
        (fn [doc]
          (js/console.log "LOADED" doc)
          (set> [ {:db/ref [ "devtest/dummy-1" :children "c"]
                   :id     "c"
                   :ts     [:db/timestamp]}]))
        (fn [result]
          (js/console.log "RESULT" result))
        u/tap>)
  )

(defn transact> [transaction>]
  (if (fn? transaction>)
    (.runTransaction
     (firestore)
     (fn [^js transaction]
       (transaction> {:get> (partial get> transaction)
                      :set> (partial set> transaction)})))
    (set> transaction>)))


(comment
  (transact> {:db/ref "devtest/dummy-2" :hello "2nd world"})
  (transact> {:db/ref "devtest/dummy-2" :db/delete true})
  (def id "dummy-3")
  (let [transaction (fn [{:keys [get> set>]}]
                      (u/=> (get> ["devtest" id])
                            (fn [dummy]
                              (js/console.log "DEBUG dummy-loaded" dummy)
                              (set> [{:firestore/path (str "devtest/" id)
                                      :counter        (inc (-> dummy :counter))}])))
                      )]
    (u/=> (transact> transaction)
          u/tap>))
  )


(comment
  (u/tap> (transact> (fn [{:keys [set>]}]
                       (set> {:db/ref "devtest/new"
                              :hello  :world})))))


(defn delete-docs> [path]
  (u/=> (get> path)
        (fn [docs]
          (transact>
           (mapv (fn [doc]
                   {:firestore/path (-> doc :firestore/path)
                    :db/delete      true})
                 docs)))))

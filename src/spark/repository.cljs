(ns spark.repository
  (:require
   [clojure.string :as str]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.firestore :as firestore]))


(defn doc> [path]
  (firestore/doc> path))

(defn col> [path]
  (firestore/col> path))

(defn query> [path]
  (log ::query>
       :path path)
  (if (-> path count even?)
    (doc> path)
    (col> path)))

(defn query-union> [paths]
  (-> (js/Promise.all (map query> paths))
      (.then (fn [results]
               (log ::query-union>--all
                    :results results)
               (->> results
                    (reduce (fn [ret docs]
                              (reduce (fn [ret doc]
                                        (assoc ret (-> doc  :id) doc))
                                      ret docs))
                            {})
                    vals
                    (js/Promise.resolve))))))

(defn create-doc> [path-or-Doc values]
  (let [doc?   (spark/doc-schema? path-or-Doc)
        id     (or (-> values :id)
                   (if doc?
                     (let [id-generator (spark/doc-schema-id-generator path-or-Doc)]
                       (id-generator {:values values}))
                     (str (random-uuid))))
        values (assoc values
                      :id id
                      :ts-created [:db/timestamp]
                      :ts-updated [:db/timestamp])
        path   (if doc?
                 [(spark/doc-schema-col-path path-or-Doc) id]
                 (if (string? path-or-Doc)
                   path-or-Doc
                   (conj path-or-Doc id)))]
    (firestore/create-doc> path values)))


(defn update-doc> [path-or-doc values]
  (let [doc?   (-> path-or-doc :firestore/path)
        values (-> values
                   (assoc :ts-updated [:db/timestamp])
                   (dissoc :id))
        path   (if doc?
                 (-> path-or-doc :firestore/path)
                 path-or-doc)]
    (if (and doc? (-> path-or-doc :firestore/exists? not))
      (create-doc> (-> path-or-doc :firestore/path) values)
      (firestore/update-fields> path values)
      )))


(defn- inner-path-as-string [path]
  (->> path
       (map #(if (keyword? %)
               (name %)
               (str %)))
       (str/join ".")))


(defn add-doc-child> [doc inner-path child-values]
  (let [child-id (get child-values :id)
        [child-id child-values] (if child-id
                                  [child-id child-values]
                                  (let [id (str (random-uuid))]
                                    [id (assoc child-values :id id)]))
        path (-> inner-path (conj child-id) inner-path-as-string)
        values {path child-values}]
    (update-doc> doc values)))


(defn update-doc-child> [doc inner-path child-id child-values]
  (let [child-values (assoc child-values
                            :id child-id
                            :ts-updated [:db/timestamp])
        values (reduce (fn [values [k v]]
                         (assoc values
                                (-> inner-path
                                    (into [child-id k])
                                    inner-path-as-string)
                                v))
                       {} child-values )]
    (update-doc> doc values)))


(defn transact-doc-update> [doc-schema doc-id update-f]
  (firestore/load-and-save>
   [(spark/doc-schema-col-path doc-schema) doc-id]
   (fn [doc]
     (let [doc (update-f doc)]
       (if (= :db/delete doc)
         :db/delete
         (assoc doc
                :id doc-id
                :ts-updated [:db/timestamp]))))))

(defn delete-doc> [doc-path]
  (firestore/delete-doc> doc-path))


(def transact> firestore/transact>)

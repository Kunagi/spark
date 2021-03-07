(ns spark.repository
  (:require
   [clojure.string :as str]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.firestore :as firestore]))


(defn query> [path]
  (log ::query>
       :path path)
  (if (-> path count even?)
    (firestore/doc> path)
    (firestore/col> path)))


(defn create-doc> [Doc values]
  (spark/assert-doc-schema Doc)
  (let [id (or (-> values :id)
               (let [id-generator (spark/doc-schema-id-generator Doc)]
                 (id-generator {:values values})))
        values (assoc values
                      :id id
                      :ts-created [:db/timestamp]
                      :ts-updated [:db/timestamp])
        path [(spark/doc-schema-col-path Doc) id]]
    (firestore/create-doc> path values)))


(defn update-doc> [doc values]
  (let [values (assoc values
                      :ts-updated [:db/timestamp])]
    (firestore/update-fields> doc values)))


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
         doc
         (assoc doc
                :id doc-id
                :ts-updated [:db/timestamp]))))))

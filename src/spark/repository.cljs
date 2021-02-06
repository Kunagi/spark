(ns spark.repository
  (:require
   [clojure.string :as str]
   [spark.logging :refer [log]]
   [spark.runtime :as runtime]
   [spark.utils :as u]
   [spark.models :as models]
   [spark.core :as spark]
   [spark.firestore :as firestore]))



(defn create-doc> [Col values]
  (let [doc-schema? (spark/doc-schema? Col)
        id (or (-> values :id)
               ((if doc-schema?
                  (spark/doc-schema-id-generator Col)
                  (models/col-id-generator Col))
                {:values values}))
        values (assoc values
                      :id id
                      :ts-created [:db/timestamp]
                      :ts-updated [:db/timestamp])
        path [(if doc-schema?
                (spark/doc-schema-col-path Col)
                (models/col-path Col)) id]]
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
  (let [values (reduce (fn [values [k v]]
                         (assoc values
                                (-> inner-path
                                    (into [child-id k])
                                    inner-path-as-string)
                                v))
                       {} child-values )]
    (update-doc> doc values)))


(defn transact-doc-update> [Col doc-id update-f]
  (firestore/load-and-save>
   [(models/col-path Col) doc-id]
   (fn [doc]
     (let [doc (update-f doc)]
       (if (= :db/delete doc)
         doc
         (assoc doc
                :id doc-id
                :ts-updated [:db/timestamp]))))))

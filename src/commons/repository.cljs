(ns commons.repository
  (:require
   [clojure.string :as str]
   [commons.logging :refer [log]]
   [commons.runtime :as runtime]
   [commons.utils :as u]
   [commons.models :as models]
   [commons.firestore :as firestore]))


(defn create-doc> [Col values]
  (let [id (or (-> values :id)
               ((models/col-id-generator Col) {:values values}))
        values (assoc values
                      :id id
                      :ts-created [:db/timestamp]
                      :ts-updated [:db/timestamp])
        path [(models/col-path Col) id]]
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
                                    [id (assoc child-id :id id)]))
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

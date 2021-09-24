(ns spark.effects
  (:require
   [spark.logging :refer [log]]
   [spark.runtime :as runtime]
   [spark.db :as db]))


(defmethod runtime/reify-effect> :db/create
  [[_ Doc values]]
  (db/add> Doc values))


(defmethod runtime/reify-effect> :db/update
  [[_ doc changes]]
  (db/update> doc changes))


(defmethod runtime/reify-effect> :db/add-child
  [[_ doc inner-path values]]
  (db/add-child> doc inner-path values))


(defmethod runtime/reify-effect> :db/update-child
  [[_ doc inner-path child-id values]]
  (let [child (get-in doc (conj inner-path child-id) values)]
    (db/update> child values)))


(defmethod runtime/reify-effect> :fn
  [[_ f]]
  (f))

(defmethod runtime/reify-effect> :log
  [[ _ data]]
  (log ::reify-effect>--log
       :data data))

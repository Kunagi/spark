(ns commons.effects
  (:require
   [commons.logging :refer [log]]
   [commons.runtime :as runtime]
   [commons.repository :as repository]))




(defmethod runtime/reify-effect> :db/create
  [[_ path values]]
  (repository/create-doc> path values))


(defmethod runtime/reify-effect> :db/update
  [[_ doc changes]]
  (repository/update-doc> doc changes))


(defmethod runtime/reify-effect> :db/add-child
  [[_ doc inner-path values]]
  (repository/add-doc-child> doc inner-path values))

(defmethod runtime/reify-effect> :db/update-child
  [[_ doc inner-path child-id values]]
  (repository/update-doc-child> doc inner-path child-id values))


(defmethod runtime/reify-effect> :fn
  [[_ f]]
  (f))

(defmethod runtime/reify-effect> :log
  [[ _ data]]
  (log ::reify-effect>--log
       :data data))

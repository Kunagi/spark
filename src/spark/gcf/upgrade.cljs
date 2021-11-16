(ns spark.gcf.upgrade
  (:require
   [spark.logging :refer [log]]
   [spark.firestore :as firestore]
   [spark.gcf :as gcf]
   ))


(defn handle-set-spa-version> [current-version ^js _req]
  (log ::handle-set-spa-version>
       :current-version current-version)
  (firestore/update-fields>
   ["sysconf" "singleton"]
   {:spa-version current-version}))


(defn exports [version]
  {

   :setSpaVersion
   (gcf/on-request--format-output> (partial handle-set-spa-version> version))

   })

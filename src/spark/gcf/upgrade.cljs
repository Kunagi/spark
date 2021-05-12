(ns spark.gcf.upgrade
  (:require
   [tick.locale-en-us]
   [spark.firestore :as firestore]
   [spark.gcf :as gcf]))


(defn handle-set-spa-version> [current-version ^js _req]
  (firestore/update-fields>
   ["sysconf" "singleton"]
   {:spa-version current-version}))


(defn exports [version]
  {

   :setSpaVersion
   (gcf/on-request--format-output> (partial handle-set-spa-version> version))

   })

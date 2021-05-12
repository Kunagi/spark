(ns spark.gcf.upgrade
  (:require
   [clojure.string :as str]
   [shadow.resource :as resource]
   [tick.locale-en-us]
   [tick.alpha.api :as tick]
   [tick.format :as tick.format]
   ["firebase-admin" :as admin]
   [spark.firestore :as firestore]
   [spark.utils :as u]
   [spark.gcf :as gcf]))


(defn handle-set-spa-version> [current-version ^js _req]
  (firestore/update-fields>
   ["sysconf" "singleton"]
   {:spa-version current-version}))


(defn exports [current-version]
  {

   :setSpaVersion
   (partial gcf/on-request--format-output> handle-set-spa-version>
            current-version)

   })

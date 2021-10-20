(ns spark.firebase-messaging-spa
  (:require
   [spark.logging :refer [log]]))

(defn messaging []
  (-> js/firebase .messaging))

;; https://firebase.google.com/docs/cloud-messaging/js/client#web-version-8
(defn get-token> [public-vapid-key]
  (log ::get-token>)
  (-> (messaging)
      (.getToken public-vapid-key)))

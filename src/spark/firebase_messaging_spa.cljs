(ns spark.firebase-messaging-spa
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]))

(defn messaging []
  (when (exists? js/firebase.messaging)
    (-> js/firebase .messaging)))

;; https://firebase.google.com/docs/cloud-messaging/js/client#web-version-8
(defn get-token> [public-vapid-key]
  (log ::get-token>)
  (if-let [messaging (messaging)]
    (-> messaging
        (.getToken public-vapid-key)
        (.catch (fn [error]
                  (log ::get-token>--error
                       :error error)
                  nil)))
    (u/resolve> nil)))

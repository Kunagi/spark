(ns spark.firebase-messaging-spa
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]))

(defn messaging []
  (when (exists? js/firebase.messaging)
    (-> js/firebase .messaging)))

;; https://firebase.google.com/docs/cloud-messaging/js/client#web-version-8
(defn get-token> [public-vapid-key]
  (let [^js messaging (messaging)]
    (log ::get-token>
         :messaging messaging)
    (if messaging
      (-> messaging
          (.getToken public-vapid-key)
          (.then (fn [token]
                   (log ::get-token>--token-received
                        :token token)
                   token)
                 (fn [error]
                   (log ::get-token>--error
                        :error error)
                   nil)))
      (u/resolve> nil))))

(defn register-on-message-handler [handler]
  (log ::register-on-message-handler
       :handler handler)
  (when-let [messaging (messaging)]
    (-> ^js messaging (.onMessage (fn [payload]
                                    (log ::on-message
                                         :payload payload)
                                    (handler (js->clj payload)))))))

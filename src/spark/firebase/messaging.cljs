(ns spark.firebase.messaging
  (:require
   ["firebase/messaging" :as firebase-messaging]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.env-config :as env-config]))

;; https://firebase.google.com/docs/cloud-messaging/js/client

(defn- initialize []
  (log ::initialize)
  (let [firebase-app (env-config/get! :firebase-app)
        service (firebase-messaging/getMessaging firebase-app)]
    ;; (when ^boolean goog.DEBUG
    ;;   (firebase-messaging/connectMessagingEmulator service "localhost" ?))
    service))

(def messaging (memoize initialize))

;; https://firebase.google.com/docs/cloud-messaging/js/client#web-version-8
(defn get-token> [public-vapid-key]
  (let [^js messaging (messaging)]
    (log ::get-token>
         :messaging messaging)
    (if messaging
      (try
        (-> (firebase-messaging/getToken messaging public-vapid-key)
            (.then (fn [token]
                     (log ::get-token>--token-received
                          :token token)
                     token)
                   (fn [error]
                     (log ::get-token>--error
                          :error error)
                     nil)))
        (catch :default ex
          (js/console.error "firebase-messaging getToken(..) failed" ex)))
      (u/resolve> nil))))

(defn register-on-message-handler [handler]
  (log ::register-on-message-handler
       :handler handler)
  (when-let [messaging (messaging)]
    (try (firebase-messaging/onMessage
          messaging (fn [payload]
                      (log ::on-message
                           :payload payload)
                      (handler (js->clj payload :keywordize-keys true))))
         (catch :default ex
           (js/console.error "firebase messaging.onMessage(..) failed" ex)))))

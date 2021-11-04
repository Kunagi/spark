(ns spark.firebase-messaging
  (:require
   ["firebase-admin" :as admin]

   [spark.logging :refer [log]]
   [spark.utils :as u]

   ))

(defn messaging-service []
  (-> ^js admin .messaging))

;; https://firebase.google.com/docs/cloud-messaging/send-message
;; https://developer.apple.com/documentation/usernotifications/setting_up_a_remote_notification_server/generating_a_remote_notification

(defn send-notification-to-device>
  [device-token title body image url badge-count data]
  (log ::send-message-do-device>
       :token device-token
       :title title
       :body body
       :image image
       :data data)
  (let [messaging-service (messaging-service)
        message (clj->js {:token device-token
                          :notification {:title title
                                         :body body
                                         :image image}
                          :webpush {:fcm_options {:link url}}
                          :apns {:payload {:aps {:sound "default"
                                                 :badge badge-count}}}
                          :data data})]
 (u/=> (-> ^js messaging-service (.send message))
          (fn [response]
            (log ::send-message-to-device>--response
                 :response response)
            response))))

(ns spark.firebase-messaging
  (:require
   ["firebase-admin" :as admin]

   [spark.logging :refer [log]]
   [spark.utils :as u]

   ))

(defn messaging-service []
  (-> ^js admin .messaging))

(defn send-notification-to-device> [device-token title body image data]
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
                          :data data})]
 (u/=> (-> ^js messaging-service (.send message))
          (fn [response]
            (log ::send-message-to-device>--response
                 :response response)
            response))   )
  )


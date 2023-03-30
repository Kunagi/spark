(ns spark.firebase.auth
  (:require
   ["firebase/auth" :as firebase-auth]
   [spark.env-config :as env-config]
   [spark.logging :refer [log]]))

;; https://firebase.google.com/docs/auth/
;; https://firebase.google.com/docs/reference/js/firebase.auth.Auth

(defn- initialize []
  (log ::initialize)
  (let [firebase-app (env-config/get! :firebase-app)
        service (firebase-auth/getAuth firebase-app)]
    ;; (when ^boolean goog.DEBUG
    ;;   (firebase-messaging/connectMessagingEmulator service "localhost" ?))
    service))

(def auth (memoize initialize))

(defn delete-user> [user]
  (firebase-auth/deleteUser user))

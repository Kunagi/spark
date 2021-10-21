(ns spark.init-gcf
  (:require
   [clojure.spec.alpha :as s]

   ["@peculiar/webcrypto" :refer [Crypto]]
   ["firebase-admin" :as admin]
   ["firebase-functions" :as functions]

   [spark.env-config :as env-config]))

(defn logging-prod-writer [event-namespace event-name event-data]
  (try
    (if event-data
      (-> functions .-logger (.debug event-namespace event-name (clj->js event-data)))
      (-> functions .-logger (.debug event-namespace event-name)))
    (catch :default ex
      (-> functions .-logger (.error "Failed to log" event-namespace event-name event-data ex)))))

(defn initialize []

  (when goog.DEBUG
    (s/check-asserts true))

  ;; @peculiar/webcrypto for nano-id
  ;; https://github.com/zelark/nano-id
  (set! js/crypto (Crypto.))

  ;; Logging
  (when-not goog.DEBUG
    (env-config/set! :logging-writer logging-prod-writer))

  ;; Firebase
  (-> admin .initializeApp)
  (env-config/set! :firestore (-> admin .firestore))

  ;;
  )

(defonce initialized
  (do
    (initialize)
    true))

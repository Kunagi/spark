(ns spark.init-gcf
  (:require
   [clojure.spec.alpha :as s]

   ["@peculiar/webcrypto" :refer [Crypto]]
   ["firebase-admin" :as admin]
   ["firebase-functions" :as functions]

   [spark.env-config :as env-config]))



(defn initialize []

  (when goog.DEBUG
    (s/check-asserts true))

  ;; @peculiar/webcrypto for nano-id
  ;; https://github.com/zelark/nano-id
  (set! js/crypto (Crypto.))


  ;; Logging
  (set! js/_spark_logger (-> functions .-logger))

  ;; Firebase
  (-> admin .initializeApp)
  (env-config/set! :firestore (-> admin .firestore))
  (env-config/set! :firebase-storage (-> admin .storage))

  ;;
  )

(defonce initialized
  (do
    (initialize)
    true))

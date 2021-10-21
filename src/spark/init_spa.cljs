(ns spark.init-spa
  (:require
   [spark.env-config :as env-config]))

(defn initialize []

  ;; Firebase
  (when goog.DEBUG
    (-> js/firebase .firestore (.useEmulator "localhost" 8080)))
  (env-config/set! :firestore (-> js/firebase .firestore))

  ;;
  )

(defonce initialized
  (do
    (initialize)
    true))

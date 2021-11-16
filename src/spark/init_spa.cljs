(ns spark.init-spa
  (:require
   [spark.utils :as u]
   [spark.env-config :as env-config]))

(defn initialize []

  ;; Firebase
  (let [firestore (-> js/firebase .firestore)]
    (u/assert firestore "Firestore not initialized")
    (when goog.DEBUG (-> firestore (.useEmulator "localhost" 8080)))
    (env-config/set! :firestore firestore))

  ;;
  )

(defonce initialized
  (do
    (try
      (initialize)
      (catch :default ex
        (js/console.error "spark.init-spa failed" ex)
        (js/setTimeout
         (fn []
           (js/window.location.reload))
         1000)))
    true))

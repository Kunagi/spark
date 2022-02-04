(ns spark.init-spa
  (:require
   [spark.utils :as u]
   [spark.env-config :as env-config]))

(defn initialize []

  (u/assert js/firebase "Firebase not loaded")

  ;; Firebase Firestore
  (u/assert js/firebase.firestore "Firebase Firestore not present")
  (let [firestore (-> js/firebase .firestore)]
    (u/assert firestore "Firebase Firestore not initialized")
    (when goog.DEBUG (-> firestore (.useEmulator "localhost" 8080)))
    (env-config/set! :firestore firestore))

  ;; Firebase Storage
  ;; (u/assert js/firebase.storage "Firebase Storage not present")
  (when js/firebase.storage
    (when-let [storage (-> js/firebase .storage)]
      (u/assert storage "Firebase Storage not initialized")
      (when goog.DEBUG (-> storage (.useEmulator "localhost" 9199)))
      (env-config/set! :firebase-storage storage)))

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
           3000)))
    true))

(ns spark.init-spa
  (:require
   ["firebase/compat/app" :default firebase]
   ["firebase/compat/firestore"]
   [spark.env-config :as env-config]
   [spark.utils :as u]))

(defn initialize [app-config]
  (let [firebase-app (-> firebase
                         (.initializeApp
                          (clj->js app-config)))]
    (env-config/set! :firebase-app firebase-app)
    (env-config/set! :firebase firebase)
    (set! js/window.firebase firebase))

  ;; Firebase Firestore
  (let [firestore (-> firebase .firestore)]
    (u/assert firestore "Firebase Firestore not initialized")
    (when goog.DEBUG (-> firestore (.useEmulator "localhost" 8080)))
    (env-config/set! :firestore firestore)))

#_(defonce initialized
    (do
      (try
        (initialize)
        (catch :default ex
          (js/console.error "spark.init-spa failed" ex)

          #_(js/setTimeout
             (fn []
               (js/window.location.reload))
             3000)))

      true))

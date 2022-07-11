(ns spark.init-spa
  (:require

   ["firebase/app" :as firebase-app]
   ;; ["firebase/compat/app" :default firebase]
   ;; ["firebase/compat/firestore"]

   [spark.utils :as u]
   [spark.env-config :as env-config]))

(defn initialize [app-config]
  (let [firebase-app (firebase-app/initializeApp (clj->js app-config))]
    (env-config/set! :firebase-app firebase-app))

  ;; Firebase Firestore
  #_(let [firestore (-> firebase .firestore)]
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

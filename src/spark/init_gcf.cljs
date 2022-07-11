(ns spark.init-gcf
  (:require
   [clojure.spec.alpha :as s]
   [cljs.pprint :refer [pprint]]
   ["fs" :as fs]

   ["@peculiar/webcrypto" :refer [Crypto]]
   ["firebase-admin/app" :as firebase-app]
   ["firebase-admin/firestore" :as firebase-firestore]
   ["firebase-functions" :as functions]

   [spark.env-config :as env-config]))

(defonce TAPS (atom '()))

(defn on-tap [value]
  (swap! TAPS conj {:ts (-> (js/Date.) .getTime)
                    :value value})
  (fs/writeFile "../../src/gcf-tap.edn"
                (with-out-str (pprint @TAPS))
                (fn [])))
(comment
  (tap> {:hello "world 2"})
  (on-tap "hello")
  (on-tap "world"))

(defn install-tap-capture []
  (add-tap on-tap))

(comment
  (tap> "hello"))

(defn initialize []

  (when goog.DEBUG
    (s/check-asserts true))

  ;; @peculiar/webcrypto for nano-id
  ;; https://github.com/zelark/nano-id
  (set! js/crypto (Crypto.))

  ;; Firebase
  (let [firebase-app (firebase-app/initializeApp)
        firestore (firebase-firestore/getFirestore)]
    (env-config/set! :firebase-app firebase-app)
    (env-config/set! :firestore firestore))
  ;; (-> admin .initializeApp)
  ;; (env-config/set! :firestore (-> admin .firestore))
  ;; (env-config/set! :firebase-storage (-> admin .storage))

  ;;
  (when goog.DEBUG
    (install-tap-capture)))

(defonce initialized
  (do
    (initialize)
    true))

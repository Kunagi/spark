(ns spark.firebase-functions
  (:require
   [spark.logging :refer [log]]))

;; https://firebase.google.com/docs/functions/callable#call_the_function
;;

(defonce REGION (atom "europe-west1"))


(defn functions []
  (let [functions (-> js/firebase
                      .app
                      (.functions @REGION))]
    (when ^boolean goog.DEBUG
      (-> functions (.useEmulator "localhost", 5001)))
    functions))


(defn call> [gcf-name data]
  (log ::call>
       :gcf-name gcf-name
       :data data)
  (let [callable (-> (functions) (.httpsCallable gcf-name))]
    (js/Promise.
     (fn [resolve reject]
       (-> (callable (clj->js data))
           (.then (fn [^js result]
                    (-> result
                        .-data
                        (js->clj :keywordize-keys true)
                        resolve)))
           (.catch reject))))))

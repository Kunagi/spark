(ns spark.firebase-functions
  (:require
   [spark.logging :refer [log]]))


(defonce REGION (atom "europe-west1"))


(when ^boolean goog.DEBUG
  (-> js/firebase
      .functions
      (.useEmulator "localhost", 5001)))


(defn functions []
  (-> js/firebase
      (.functions @REGION)))


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

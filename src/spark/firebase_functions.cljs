(ns spark.firebase-functions
  (:require
   [spark.logging :refer [log]]))


(when ^boolean goog.DEBUG
  (-> js/firebase
      .functions
      (.useEmulator "localhost", 5001)))


(defn functions []
  (-> js/firebase
      .functions))


(defn call> [gcf-name data]
  (log ::call>
       :gcf-name gcf-name
       :data data)
  (let [callable (-> (functions) (.httpsCallable gcf-name))]
    (js/Promise.
     (fn [resolve _reject]
       (-> (callable (clj->js data))
           (.then (fn [^js result]
                    (-> result
                        .-data
                        (js->clj :keywordize-keys true)
                        resolve))))))))

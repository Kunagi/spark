(ns spark.firebase.functions
  (:require
   ["firebase/functions" :as firebase-functions]
   [spark.logging :refer [log]]
   [spark.env-config :as env-config]))

;; https://firebase.google.com/docs/functions/callable#call_the_function

(defonce REGION (atom "europe-west1"))

(defn- initialize []
  (log ::initialize)
  (let [firebase-app (env-config/get! :firebase-app)
        service (firebase-functions/getFunctions firebase-app @REGION)]
    (when ^boolean goog.DEBUG
      (firebase-functions/connectFunctionsEmulator
       service "localhost" 5001))
    service))

(def functions (memoize initialize))

(defn call> [gcf-name data]
  (log ::call>
       :gcf-name gcf-name
       :data data)
  (let [callable (firebase-functions/httpsCallable (functions) gcf-name)
        ;; callable (-> (functions) (.httpsCallable gcf-name))
        ]
    (js/Promise.
     (fn [resolve reject]
       (-> (callable (clj->js data))
           (.then (fn [^js result]
                    (let [result (-> result
                                     .-data
                                     (js->clj :keywordize-keys true))]
                      (log ::call>--completed
                           :result result)
                      (resolve result))))
           (.catch (fn [error]
                     (log ::call>--failed
                          :error error)
                     (reject (ex-info (str "Call to " gcf-name " failed")
                                      {:gcf-name gcf-name
                                       :data     data}
                                      error)))))))))

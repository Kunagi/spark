;; * ns
(ns spark.gcf
  (:require
   ["firebase-functions" :as functions]
   [cljs.pprint :refer [pprint]]
   [clojure.string :as str]
   [shadow.resource :as resource]
   [spark.firestore :as firestore]
   [spark.logging :refer [log]]
   [spark.utils :as u]))

;; * config
;; https://firebase.google.com/docs/functions/config-env

(def default-run-with-opts {:memory "1GB"})

(defn config
  ([path]
   (reduce (fn [^js o path-item]
             (when o
               (aget o (if (keyword? path-item)
                         (name path-item)
                         (str path-item)))))
           (config) path))
  ([]
   (-> functions .config)))

(comment
  (js/console.log (-> functions .config)))

;; * response formatting

(defn- wrap-in-html [text]
  (str
   "<!DOCTYPE html>
<html>
  <body>
    <div style=\"white-space: pre-wrap; font-family: monospace;\">
" text "
    </div>
  </body>
</html>"))

(defn exception-text [ex]
  (when ex
    (str (ex-message ex)
         (when-let [data (ex-data ex)]
           (str "\n\n  data:\n\n"
                (u/->edn (ex-data ex))))
         (when-let [cause (ex-cause ex)]
           (str "\n\n  caused by:\n\n" (exception-text cause))))))

(defn- format-response [val]
  ;; (js/console.log "!!!" (instance? cljs.core.ExceptionInfo val))
  (cond
    (instance? cljs.core.ExceptionInfo val)
    (wrap-in-html (exception-text val))

    (or (nil? val) (sequential? val) (map? val) (vector? val) (list? val))
    (wrap-in-html (with-out-str (pprint val)))

    (or (string? val) (number? val))
    (wrap-in-html (str val))

    :else (str val)
    ;; :else (js/JSON.stringify val)
    ))
;; * region

(defn region--europe-west1 [run-with-opts]
  (let [function-builder ^js (-> functions
                                 (.region "europe-west1"))
        function-builder (-> function-builder
                             (.runWith (clj->js (merge default-run-with-opts run-with-opts))))]
    function-builder))

(defn region--us-central1 [run-with-opts]
  (let [function-builder ^js (-> functions
                                 (.region "us-central1"))
        function-builder (-> function-builder
                             (.runWith (clj->js (merge default-run-with-opts run-with-opts))))]
    function-builder))

;; * on-request

(defn on-request
  ([handler]
   (on-request handler nil))
  ([handler run-with-opts]
   (-> ^js (region--europe-west1 run-with-opts)
       .-https
       (.onRequest handler))))

(defn on-request>
  ([handler>]
   (on-request> handler> nil))
  ([handler> run-with-opts]
   (on-request
    (fn [req res]
      (-> (u/as> (handler> req))
          (.then #(-> res
                      (.set "Access-Control-Allow-Origin" "*")
                      (.status 200)
                      (.send %)))))
    run-with-opts)))

(defn on-request--format-output> [handler>]
  (on-request
   (fn [^js req ^js res]
     (try
       (-> (u/as> (handler> req))
           (.then #(-> res
                       (.set "Access-Control-Allow-Origin" "*")
                       (.status 200)
                       (.send (format-response %)))
                  (fn [error]
                    (js/console.error error)
                    (log ::request-hander-failed
                         :exception error)
                    (-> res
                        (.set "Access-Control-Allow-Origin" "*")
                        (.status 500)
                        (.send (str "<h1>Error</h1>\n\n"
                                    (format-response (str error))))))))
       (catch :default ex
         (js/console.error ex)
         (log ::request-hander-failed
              :exception ex)
         (-> res
             (.set "Access-Control-Allow-Origin" "*")
             (.status 500)
             (.send (str "<h1>Error</h1>\n\n"
                         (format-response ex)))))))))

;; * on-schedule

;; https://firebase.google.com/docs/functions/schedule-functions
;; https://firebase.google.com/docs/reference/functions/function_configuration_.schedule

(defn on-schedule [schedule-pattern handler> run-with-opts]
  (-> ^js (region--europe-west1 run-with-opts)
      .-pubsub
      (.schedule schedule-pattern)
      (.timeZone "Europe/Berlin")
      (.onRun (fn [^js context]
                (handler> context)))))

;; * on-call

;; https://firebase.google.com/docs/functions/callable
;; https://firebase.google.com/docs/reference/functions/providers_https_#oncall

(defn handle-on-call-result [result]
  (if (u/promise? result)
    (js/Promise.
     (fn [resolve reject]
       (log ::handle-on-call-result--1)
       (-> result
           (.then (fn [result]
                    (resolve (handle-on-call-result result)))
                  (fn [error]
                    #_(-> functions
                          .-logger
                          (.error (str "Error in GCF call:")
                                  error))
                    (log ::handle-on-call-result--error
                         :error error
                         :error-data (ex-data error))
                    (resolve (clj->js {:error "Error in cloud function. See logs for details."})))))))
    (try
      (clj->js result)      
      (catch js/Error ex
        (log ::handle-on-call-result--error--clj->js
             :result result)
        nil
        #_(str result)))))

(defn on-call [handler run-with-opts]
  (-> (region--europe-west1 run-with-opts)
      (.runWith (clj->js {:minInstances 1}))
      .-https
      (.onCall (fn [^js data ^js context]
                 (handle-on-call-result
                  (handler (js->clj data :keywordize-keys true)
                           context))))))

;; * on-doc....

;; https://firebase.google.com/docs/functions/database-events

(defn on-doc-update [path handler> run-with-opts]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (-> (region--europe-west1 run-with-opts)
        .-firestore
        (.document path-s)
        (.onUpdate (fn [^js change ^js context]
                     (let [before-doc (-> change .-before firestore/wrap-doc)
                           after-doc  (-> change .-after firestore/wrap-doc)]
                       (-> (u/as> (handler> before-doc after-doc context))
                           (.then (fn [result]
                                    result)
                                  (fn [error]
                                    (log ::on-doc-update--error
                                         :error error
                                         :error-data (ex-data error)
                                         :path path-s)
                                    nil)))))))))

(defn on-doc-write [path handler> run-with-opts]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (->  ^js (region--europe-west1 run-with-opts)
         .-firestore
         (.document path-s)
         (.onWrite (fn [^js change ^js context]
                     (let [before-doc (-> change .-before firestore/wrap-doc)
                           after-doc  (-> change .-after firestore/wrap-doc)]
                       (-> (u/as> (handler> before-doc after-doc context))
                           (.then (fn [result]
                                    result)
                                  (fn [error]
                                    (log ::on-doc-write--error
                                         :error error
                                         :error-data (ex-data error)
                                         :path path-s)
                                    nil)))))))))

(defn on-doc-create [path handler> run-with-opts]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (-> ^js (region--europe-west1 run-with-opts)
        .-firestore
        (.document path-s)
        (.onCreate (fn [^js doc ^js context]
                     (let [doc (-> doc firestore/wrap-doc)]
                       (-> (u/as> (handler> doc context))
                           (.then (fn [result]
                                    result)
                                  (fn [error]
                                    (log ::on-doc-create--error
                                         :error error
                                         :error-data (ex-data error)
                                         :path path-s)
                                    nil)))))))))

;; * storage

(defn on-storage-object-finalize [handler> run-with-opts]
  (-> (region--europe-west1 run-with-opts)
      .-storage
      .object
      (.onFinalize (fn [^js object]
                     (u/as> (handler> object))))))

;; * errors

;; https://firebase.google.com/docs/reference/functions/providers_https_.httpserror
(defn throw-error [error-code error-message]
  (let [HttpsError (-> functions .-https .-HttpsError)]
    (throw (HttpsError. error-code error-message))))

(defn throw-error--unauthenticated []
  (throw-error "unauthenticated" nil))

(defn throw-error--failed-precondition [message]
  (throw-error "failed-precondition" message))

(defn throw-error--invalid-argument [message]
  (throw-error "invalid-argument" message))

(defn throw-error--permission-denied [message]
  (throw-error "permission-denied" message))


;; * Version

(defn current-version []
  (str/trim (str (resource/inline "spa/version.txt"))))

(defn current-version-time []
  (str/trim (str (resource/inline "spa/version-time.txt"))))

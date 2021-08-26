;; * ns
(ns spark.gcf
  (:require
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]

   [tick.locale-en-us]
   [tick.timezone]

   ["firebase-functions" :as functions]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.firestore :as firestore]))

;; * config
;; https://firebase.google.com/docs/functions/config-env

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
</html>")
  )

(defn exception-text [ex]
  (when ex
    (str (ex-message ex)
         (when-let [data (ex-data ex)]
           (str "\n\n  data:\n\n"
                (u/->edn (ex-data ex))))
         (when-let [cause (ex-cause ex)]
           (str "\n\n  caused by:\n\n" (exception-text cause))))))

(defn- format-response [val]
  (js/console.log "!!!" (instance? cljs.core.ExceptionInfo val))
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

(defn region--europe-west1 []
  (-> functions (.region "europe-west1")))

;; * on-request

(defn on-request [handler]
  (-> (region--europe-west1)
      .-https
      (.onRequest handler)))


(defn on-request> [handler>]
  (on-request
   (fn [req res]
     (-> (handler> req)
         (.then #(-> res (.status 200) (.send %)))))))


(defn on-request--format-output> [handler>]
  (on-request
   (fn [^js req ^js res]
     (try
       (-> (u/as> (handler> req))
           (.then #(-> res
                       (.set "Access-Control-Allow-Origin" "*")
                       (.status 200)
                       (.send (format-response %)))
                  #(-> res
                       (.set "Access-Control-Allow-Origin" "*")
                       (.status 500)
                       (.send (str "<h1>Error</h1>\n\n"
                                   (format-response (str %)))))))
       (catch :default ex
         (log ::request-hander-failed
              ex)
         (-> res
             (.set "Access-Control-Allow-Origin" "*")
             (.status 500)
             (.send (str "<h1>Error</h1>\n\n"
                         (format-response ex))))
         )))))

;; * on-schedule

;; https://firebase.google.com/docs/functions/schedule-functions
;; https://firebase.google.com/docs/reference/functions/function_configuration_.schedule

(defn on-schedule [schedule-pattern handler>]
  (-> (region--europe-west1)
      .-pubsub
      (.schedule schedule-pattern)
      (.timeZone "Europe/Berlin")
      (.onRun (fn [^js context]
                (handler> context)))
      ))

;; * on-call

;; https://firebase.google.com/docs/functions/callable
;; https://firebase.google.com/docs/reference/functions/providers_https_#oncall

(defn handle-on-call-result [result]
  (if (instance? js/Promise result)
    (js/Promise.
     (fn [resolve reject]
       (-> result
           (.then #(resolve (handle-on-call-result %)))
           (.catch (fn [error]
                     (log ::call-handler-failed error)
                     (reject error))))))
    (clj->js result)))

(defn on-call [handler]
  (-> (region--europe-west1)
      .-https
      (.onCall (fn [^js data ^js context]
                 (handle-on-call-result
                  (handler (js->clj data :keywordize-keys true)
                           context))))))

;; * on-doc....

;; https://firebase.google.com/docs/functions/database-events

(defn on-doc-update [path handler>]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (-> (region--europe-west1)
        .-firestore
        (.document path-s)
        (.onUpdate (fn [^js change ^js context]
                     (let [before-doc (-> change .-before firestore/wrap-doc)
                           after-doc  (-> change .-after firestore/wrap-doc)]
                       (handler> before-doc after-doc context)))))))

(defn on-doc-write [path handler>]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (-> (region--europe-west1)
        .-firestore
        (.document path-s)
        (.onWrite (fn [^js change ^js context]
                    (let [before-doc (-> change .-before firestore/wrap-doc)
                          after-doc  (-> change .-after firestore/wrap-doc)]
                      (handler> before-doc after-doc context)))))))

(defn on-doc-create [path handler>]
  (let [path-s (->> path
                    (map #(if (keyword? %)
                            (str "{" (name %) "}")
                            %))
                    (str/join "/"))]
    (-> (region--europe-west1)
        .-firestore
        (.document path-s)
        (.onCreate (fn [^js doc ^js context]
                     (let [doc (-> doc firestore/wrap-doc)]
                       (handler> doc context)))))))


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

;; * ns
(ns spark.gcf
  (:require
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]

   ["firebase-functions" :as functions]

   [spark.firestore :as firestore]))

;; * config

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

(defn- format-response [val]
  (cond
    (or (nil? val) (sequential? val) (map? val) (vector? val) (list? val))
    (wrap-in-html (with-out-str (pprint val)))

    (or (string? val) (number? val))
    (wrap-in-html (str val))

    ;; :else (str val)
    :else (js/JSON.stringify val)
    ;; :else (str val)
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
   (fn [req res]
     (-> (handler> req)
         (.then #(-> res
                     (.set "Access-Control-Allow-Origin" "*")
                     (.status 200)
                     (.send (format-response %)))
                (when (-> req .-query .-debug)
                  #(-> res
                       (.set "Access-Control-Allow-Origin" "*")
                       (.status 500)
                       (.send (str "<h1>Error</h1>\n\n"
                                   (format-response (str %)))))))))))

;; * on-schedule

;; https://firebase.google.com/docs/functions/schedule-functions

(defn on-schedule [schedule-pattern handler>]
  (-> (region--europe-west1)
      .-pubsub
      (.schedule schedule-pattern)
      (.onRun (fn [^js context]
                (handler> context)))
      ))

;; * on-call

;; https://firebase.google.com/docs/functions/callable
;; https://firebase.google.com/docs/reference/functions/providers_https_#oncall

(defn handle-on-call-result [result]
  (if (instance? js/Promise result)
    (js/Promise.
     (fn [resolve _reject]
       (-> result
           (.then #(resolve (handle-on-call-result %))))))
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

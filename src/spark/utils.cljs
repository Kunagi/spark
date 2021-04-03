(ns spark.utils
  (:refer-clojure :exclude [pos? zero? min max tap>])
  (:require
   [clojure.spec.alpha :as s]
   [cljs.pprint :refer [pprint]]

   [goog.object :as gobj]

   [malli.core :as malli]
   [malli.error :as malli-error]))


;; http://weavejester.github.io/medley/medley.core.html

;;; tap

(defn tap> [value]
  (if (instance? js/Promise value)
    (-> value (.then clojure.core/tap>))
    (clojure.core/tap> value))
  value)

(comment
  (tap> "hello world")
  (tap> {:name "Witek"})
  (tap> (js/Promise.resolve {:name "Witek"})))

;;; fetch

(defn fetch>
  ([url]
   (fetch> url {}))
  ([url opts]
   (js/fetch url (-> opts clj->js))))

(defn fetch-json>
  ([url]
   (fetch-json> url {})
   )
  ([url opts]
   (-> (fetch> url opts)
       (.then (fn [^js response]
                (-> response .json)))
       (.then (fn [^js json]
                (js->clj json :keywordize-keys true))))))

;;; maps

(defn deep-merge
  "Recursively merges maps together. If all the maps supplied have nested maps
  under the same keys, these nested maps are merged. Otherwise the value is
  overwritten, as in `clojure.core/merge`."
  {:arglists '([& maps])}
  ([])
  ([a] a)
  ([a b]
   (when (or a b)
     (letfn [(merge-entry [m e]
               (let [k  (key e)
                     v' (val e)]
                 (if (contains? m k)
                   (assoc m k (let [v (get m k)]
                                (if (and (map? v) (map? v'))
                                  (deep-merge v v')
                                  v')))
                   (assoc m k v'))))]
       (reduce merge-entry (or a {}) (seq b)))))
  ([a b & more]
   (reduce deep-merge (or a {}) (cons b more))))


(defn dissoc-nil-vals
  "Dissoc all keys in `m` where the value is `nil?`."
  [m]
  (reduce (fn [m [k v]]
            (if (nil? v)
              (dissoc m k)
              m))
          m m))

(defn assoc-if-not=
  "Assoc if the new value `v` ist not `=` to the current value in `m`."
  [m k v]
  (if (= v (get m k))
    m
    (assoc m k v)))


(defn assoc-if-missing
  "Assoc if `m` is missing key `k`."
  [m k v]
  (if (= ::missing (get m k ::missing))
    (assoc m k v)
    m))


(defn assoc-if-not-nil
  "Assoc if the value `v` ist not nil."
  [m k v]
  (if (nil? v)
    m
    (assoc m k v)))

(defn index-by
  "Returns a map of the elements of coll keyed by the result of f on each
  element. The value at each key will be the last element in coll associated
  with that key. This function is similar to `clojure.core/group-by`, except
  that elements with the same key are overwritten, rather than added to a
  vector of values."
  [f coll]
  (persistent! (reduce #(assoc! %1 (f %2) %2) (transient {}) coll)))

;;; vectors

(defn v-contains?
  "Checks if `vec` contains `elem`."
  [vec elem]
  (->> vec
       (some #(= % elem))
       boolean))


;;; functions

(defn trampoline-if
  "Calls `trampoline` if `fn-or-value` is `fn?`,
  otherwise returns `fn-or-value`."
  [fn-or-value]
  (if (fn? fn-or-value)
    (trampoline fn-or-value)
    fn-or-value))


(defn fn->value
  [fn-or-value & args]
  (if (fn? fn-or-value)
    (apply fn-or-value args)
    fn-or-value))


(defn safe-apply [f args]
  (if f
    (apply f args)
    (first args)))


(defn update-if
  [v f & more-args]
  (if f
    (apply f (into [v] more-args))
    v))

;;; strings

(defn string-pad-left [s min-len padding]
  (when s
    (let [s (if ( string? s) s (str s))
          padding (if ( string? padding) padding (str padding))]
      (if (-> padding count (= 0))
        s
        (if (-> s count (>= min-len))
          s
          (recur (str padding s) min-len padding))))))

(comment
  (string-pad-left "1" 3 "0")
  (string-pad-left 1 3 0)
  (string-pad-left "100" 3 "0")
  (string-pad-left "1" 3 nil)
  (string-pad-left "1" 3 ""))

;;; edn

(defn ->edn [data]
  (with-out-str (pprint data)))

;;; date and time

(defn millis [thing]
  (cond
    (nil? thing) nil
    (number? thing) thing
    (instance? js/Date thing) (-> thing .getTime)
    :else (js/Date.parse thing)))

(comment
  (js/Date. (millis "2020-01-01"))
  (js/Date. (millis 1577870520000))
  (js/Date. (millis (js/Date.))))


(defn timespans-overlapping? [a-start a-end b-start b-end]
  (not
   (or (<= a-end b-start)               ; a is before b / a ends before b start
       (<= b-end a-start)               ; b is before a / b ends before a starts
       )))

(defn timestamp [date-string]
  (when date-string
    (if (instance? js/Date date-string)
      date-string
      (js/Date. (js/Date.parse date-string)))))

(comment
  (timestamp "2020-01-01"))

(defn date [date-or-string]
  (when date-or-string
    (let [ts (timestamp date-or-string)]
      (str (-> ts .getFullYear)
           "-"
           (-> ts .getMonth inc (string-pad-left 2 "0"))
           "-"
           (-> ts .getDate (string-pad-left 2 "0"))))))

(comment
  (timestamp "2020-01-01 10:22")
  (date "2020-01-01 10:22"))

(defn date-today []
  (date (js/Date.)))

(comment
  (date-today))

(defn date-same-day? [date-a date-b]
  (let [date-a (timestamp date-a)
        date-b (timestamp date-b)]
    (and (= (-> date-a .getDate)   (-> date-b .getDate))
         (= (-> date-a .getMonth) (-> date-b .getMonth))
         (= (-> date-a .getFullYear)  (-> date-b .getFullYear)))))

(comment
  (date-same-day? (js/Date.) (js/Date.))
  (date-same-day? "2020-01-01" "2020-01-01")
  (date-same-day? "2020-01-01" "2020-01-02"))


(defn date-before? [date test-date]
  (let [date (timestamp date)
        test-date (timestamp test-date)]

    (cond
      (< (-> date .getFullYear) (-> test-date .getFullYear)) true
      (> (-> date .getFullYear) (-> test-date .getFullYear)) false
      :else (cond
              (< (-> date .getMonth) (-> test-date .getMonth)) true
              (> (-> date .getMonth) (-> test-date .getMonth)) false
              :else (cond
                      (< (-> date .getDate) (-> test-date .getDate)) true
                      (> (-> date .getDate) (-> test-date .getDate)) false
                      :else false)))))

(comment
  (date-before? "2020-01-01" "2020-01-02")
  (date-before? "2020-01-01" "2020-02-01")
  (date-before? "2020-01-01" "2021-01-01")
  (date-before? "2020-01-01" "2020-01-01")
  (date-before? "2020-01-02" "2020-01-01")
  (date-before? "2020-02-01" "2020-01-01")
  (date-before? "2022-01-01" "2020-01-01"))

(defn date-past? [date]
  (date-before? date (date-today)))

(comment
  (date-today)
  (date-past? (js/Date.))
  (date-past? (date-today))
  (date-past? "2021-04-01")
  (date-past? "2021-04-02")
  (date-past? "2021-04-03"))

;;; promises

(defn resolve> [result]
  (js/Promise.resolve result))

(defn reject> [result]
  (js/Promise.reject result))

(defn no-op> []
  (js/Promise.resolve nil))

(defn as>
  "Converge `thing` to js/Promise."
  [thing]
  (cond
    (nil? thing)
    (no-op>)

    (instance? js/Promise thing)
    thing

    :else
    (resolve> thing)))

(defn all> [& promises-or-lists-of-promises]
  (js/Promise.all
   (reduce (fn [promises promise-or-list]
             (if (instance? js/Promise promise-or-list)
               (conj promises promise-or-list)
               (->> promise-or-list
                    (map as>)
                    (into promises))))
           [] promises-or-lists-of-promises)))

(defn later> [wait-millis f]
  (js/Promise.
   (fn [resolve _]
     (js/setTimeout #(resolve (f))
                    wait-millis))))

(defn => [promise & thens]
  (reduce (fn [promise then]
            (-> promise
                (.then then)))
          (as> promise) thens))

(comment
  (-> (resolve> {:log []})
      (.then #(update % :log conj "a"))
      (.then tap>)
      (.then #(later> 1000 (fn [] (update % :log conj "b"))))
      (.then #(update % :log conj "c"))
      (.then #(resolve> (update % :log conj "d")))
      (.then tap>))

  (=> {:log []}
      #(update % :log conj "a")
      #(reject> "boom")
      tap>
      #(later> 1000 (fn [] (update % :log conj "b")))
      #(update % :log conj "c")
      #(resolve> (update % :log conj "d"))
      tap>))

(defn transform>
  "Returns `js/Promise` which resolves the application of `transform` on the
  value of `promise`.

  Use this if you have a promise which value needs to be transformed."
  [promise transform]
  (js/Promise.
   (fn [resolve reject]
     (-> promise
         (.then (fn [result]
                  (let [transformed (transform result)]
                    (resolve transformed)))
                reject)))))


(defn p-transformed
  "Wraps promise function `f>` with `transform` function.

  Use this if you have a promise function which value needs to be transformed."
  [f> transform]
  (fn [& args]
    (transform> (apply f> args) transform)))


(defn chain-promise-fns> [input-value fns]
  (let [fns (remove nil? fns)]
    (if-let [fn> (first fns)]
      (-> (fn> input-value)
          (.then #(chain-promise-fns> % (rest fns))))
      (js/Promise.resolve input-value))))


(defn apply>
  "Returns `js/Promise` with the result of applying `f` on `args` while
  resolving all promises in `args`."
  [f args]
  (s/assert vector? args)
  (-> (js/Promise.all args)
      (.then #(js/Promise.resolve (apply f %)))))


(comment
  (instance? js/Promise (js/Promise. (fn [_ _])))
  (let [sum (fn [a b c] (+ a b c))]
    (js/console.log "direct invocation:" (sum 1 2 3))
    (-> (apply> sum [1 2 3])
        (.then #(js/console.log "promise result #1:" %)))))


;;; deprecations

(defn log-deprecated [info]
  (js/console.error "DEPRECATED" (js/Error info)))


;;; malli


(defn malli-explain->user-message [explain schema]
  (when explain
    (str "Value does not match schema: "
         (try
           (malli-error/humanize explain)
           (catch :default ex
             (throw (ex-info "Malli schema error humanization failed."
                             {:schema schema
                              :explain explain}
                             ex)))))))

(defn malli-explain [schema value]
  (try
    (malli/explain schema value)
    (catch :default ex
      (throw (ex-info "Invalid malli schema"
                      {:malli/schema schema}
                      ex)))))

(defn assert-malli [schema value]
  (when-let [explain (malli-explain schema value)]
    (throw (ex-info (malli-explain->user-message explain schema)
                    {:malli/explain explain})))
  value)


(defn malli-map-field-by-id [schema field-key]
  (when schema
    (->> schema
         (filter #(and (vector? %) (= field-key (first %))))
         first)))


(defn malli-map-field-schema-by-id [schema field-key]
  (when-let [field (malli-map-field-by-id schema field-key)]
    (let [field-schema (second field)]
      (if (map? field-schema)
        (nth field 2)
        field-schema))))


(defn conform-js-data [^js data schema]
  (cond

    (= :keyword schema)
    (keyword data)

    (or (nil? data) (string? data) (number? data) (boolean? data))
    (js->clj data)

    (instance? (if (exists? js/firebase)
                 (-> js/firebase.firestore.Timestamp)
                 (-> (js/require "firebase-admin") .-firestore .-Timestamp))
               data)
    (-> data .toDate)

    ^boolean (js/Array.isArray data)
    (case (first schema)
      :set
      (into #{} (map #(conform-js-data % (second schema)) data))

      :vector
      (mapv #(conform-js-data % (second schema)) data)

      (mapv #(conform-js-data % nil) data))

    (vector? schema)
    (case (first schema)

      :map-of
      (reduce (fn [m js-key]
                (let [k js-key
                      v (gobj/get data js-key)
                      v-schema (if (map? (second schema))
                                 (nth schema 3)
                                 (nth schema 2))]
                  (assoc m k (conform-js-data v v-schema))))
              {} (js/Object.keys data))

      (reduce (fn [m js-key]
                (let [k (keyword js-key)
                      v (gobj/get data js-key)]
                  (assoc m k (conform-js-data v (malli-map-field-schema-by-id schema k)))))
              {} (js/Object.keys data)))

    :else
    (js->clj data :keywordize-keys true)))

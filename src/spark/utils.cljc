;; * ns
(ns spark.utils
  (:refer-clojure :exclude [assert pos? zero? min max tap>])
  #?(:cljs (:require-macros [spark.utils :refer [assert try>]]))
  (:require
   #?(:clj [clojure.pprint :refer [pprint]]
      :cljs [cljs.pprint :refer [pprint]])
   #?(:cljs ["json-prune" :as json-prune])
   [shadow.resource :as resource]
   [clojure.string :as str]
   [flatland.ordered.map :as ordered.map]
   [kunagi.utils :as u]
   [malli.core :as malli]
   [malli.error :as malli-error]
   [nano-id.core :as nano-id]
   [spark.time :as time]))

;; http://weavejester.github.io/medley/medley.core.html

;; * macros

#?(:clj
   (defmacro assert [assertion & [message data]]
     (let [assertion-formated (with-out-str (pprint assertion))
           assertion-formated (-> assertion-formated (str/replace "\n" " "))
           in-function (str (-> &env :fn-scope first :info :ns) "." (-> &env :fn-scope first :name))
           in-file (-> &env :ns :meta :file)
           in-line (-> &env :line)]
       `(let [result# ~assertion]
          (when-not result#
            (spark.logging/log ::assert--failed
                               :message ~message
                               :data ~data)
            (throw (ex-info (str
                             (when ~message (str  ~message " | "))
                             "Assertion failed. | "
                             ~assertion-formated
                             "| in " ~in-function " (" ~in-file ":" ~in-line ")")
                            (assoc ~data
                                   :value result#
                                   :assertion-form '~assertion
                                   :in-function ~in-function
                                   :in-line ~in-line
                                   :in-file ~in-file))))
          result#))))

#?(:clj
   (defmacro get-env-example []
     (let [s (str &env)]
       `~s)))

#?(:clj
   (defmacro get-compiler-option-example []
     (let [s (with-out-str (pprint (get-in @cljs.env/*compiler* [:options])))]
       `~s)))

#?(:clj
   (defmacro try> [& body] `(u/try> ~@body)))

;; * tap

#?(:cljs (defn tap> [value]
           (let [start-time (-> (js/Date.) .getTime)]
             (if (instance? js/Promise value)
               (-> value (.then (fn [result]
                                  (clojure.core/tap> {:promise/resolved result
                                                      :promise/runtime (- (-> (js/Date.) .getTime)
                                                                          start-time)}))
                                (fn [error]
                                  (clojure.core/tap> {:promise/rejected error
                                                      :promise/runtime (- (-> (js/Date.) .getTime)
                                                                          start-time)}))))
               (clojure.core/tap> value)))
           value)
   :clj (def tap> clojure.core/tap>))

(comment
  (tap> "hello world")
  (tap> {:name "Witek"})
  (tap> (js/Promise.resolve {:name "Witek"})))

;; * assert
;;  assert.clj

(comment
  (pprint [:a :b])
  (macroexpand '(assert :x)))

;; JSON

#?(:cljs
   (defn js->json [js-obj]
     (when js-obj
       (try
         #_(js/JSON.stringify
            js-obj
            (fn [k v]
              v)
            2)
         (json-prune js-obj
                     (clj->js {
                               ;; :replacer (fn [value default-value]
                               ;;             (cond
                               ;;               :else default-value))
                               }))
         (catch :default _ex
           (js/console.error _ex)
           nil
           #_(str js-obj))))))

#?(:cljs (defn ->json [o]
           (when o
             (-> o
                 clj->js
                 js->json))))

(comment
  (js/console.log (->json {:hello "world"
                           :x js/undefined
                           :window js/window}))
  (js/console.log "boo"))

;; * EDN

(def ->edn u/->edn)
(def read-edn u/read-edn)

;; Errors and Exceptions

(def error->data u/error->data)
(def error->text u/error->text)
(def exception-as-text u/error->text)

;; * unique ids

(defn nano-id []
  (let [id (nano-id/nano-id)]
    (if (re-matches #"[a-zA-Z][a-zA-Z0-9]*" id)
      id
      (nano-id))))

(comment
  (nano-id))

;; * fetch

#?(:cljs
   (defn fetch>
     ([url]
      (fetch> url {}))
     ([url opts]
      (js/fetch url (-> opts clj->js)))))

#?(:cljs
   (defn fetch-json>
     ([url]
      (fetch-json> url {}))
     ([url opts]
      (-> (fetch> url opts)
          (.then (fn [^js response]
                   (-> response .json)))
          (.then (fn [^js json]
                   (js->clj json :keywordize-keys true)))))))

;; * numbers

#?(:cljs
   (defn parse-float [v]
     (when v
       (let [f (js/parseFloat v)]
         (when (js/isNaN f) (throw (ex-info (str "NaN: " v)
                                            {:value v})))
         f))))

(comment
  (parse-float "22.2")
  (parse-float "22.2x")
  (parse-float "c22.2"))

;; * maps

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

(defn assoc-if-nil
  "Assoc if `m` is missing key `k` or its value is `nil`."
  [m k v]
  (if (get m k)
    m
    (assoc m k v)))

(defn assoc-if-not-nil
  "Assoc if the value `v` ist not nil."
  [m k v]
  (if (nil? v)
    m
    (assoc m k v)))

(defn assoc-if-not=-in-other-map
  [m k other-map v]
  (if (= (get other-map k) v)
    m
    (assoc m k v)))

(defn assoc-if-missing-in-other-map
  [m k other-map v]
  (if (= ::missing (get other-map k ::missing))
    (assoc m k v)
    m))

(defn assoc-if-nil-in-other-map
  [m k other-map v]
  (if (get other-map k ::missing)
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

;; * sequences

(defn seq-contains-nil? [s]
  (when s
    (->> s
         (filter nil?)
         seq
         boolean)))

(comment
  (seq-contains-nil? [nil])
  (seq-contains-nil? [:a nil])
  (seq-contains-nil? [:a])
  (seq-contains-nil? nil)
  (seq-contains-nil? []))

(defn seq-contains? [s v]
  (contains? (into #{} s) v))

(defn seq-indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn seq-positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (seq-indexed coll) :when (pred elt)] idx))

(defn seq-position [pred coll]
  (first (seq-positions pred coll)))

(comment
  (seq-position #{:b} [:a :b :c])
  (seq-position #{:b} [:a])
  (seq-position #{:b} nil))

#?(:cljs
   (defn distribute
     "Like `partition` but with distributed elements."
     [n coll]
     (if (< n 2)
       [coll]
       (let [coll-size (count coll)
             partition-size (js/Math.ceil (/ (count coll) n))
             needed-coll-size (* n partition-size)
             coll (if (< coll-size needed-coll-size)
                    (take needed-coll-size (concat coll (repeat nil)))
                    coll)
             rows (partition n coll)
             interleaved (apply interleave rows)
             partitions (partition-all partition-size interleaved)
             partitions (map #(remove nil? %) partitions)
             partitions (remove empty? partitions)]
         partitions))))

(comment
  (/ 5 2)
  (interleave [:a :b] [:c :d] [:e :f])
  (interleave [:a :c] (concat  [:b] (repeat nil)))
  (distribute 2 [:a :b :c :d :e :f]) ; [[:a :c :e][:b :d :f]]
  (distribute 2 [:a :b :c :d :e]) ; [[:a :c :e] [:b :d]]
  (distribute 3 [:a :b :c :d :e :f])
  (distribute 3 [:a :b :c :d :e]) ; [[:a :d] [:b :e] [:c]]
  (distribute 1 [:a :b :c :d :e])
  (distribute 2 [])
  (distribute 3 [:a])
  ;;
  )

;; ** vectors

(defn v-contains?
  "Checks if `vec` contains `elem`."
  [vec elem]
  (->> vec
       (some #(= % elem))
       boolean))

(defn v-remove
  [pred coll]
  (when coll
    (vec (remove pred coll))))

(comment
  (v-remove number? [1 "2" :3])
  (v-remove number? nil)
  (v-remove number? '()))

(defn v-remove-at
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn v-assoc-last [coll val]
  (if (seq coll)
    (let [idx (-> coll count dec)]
      (assoc coll idx val))
    [val]))

(comment
  (v-assoc-last [0 1] :x)
  (v-assoc-last [0] :x)
  (v-assoc-last nil :x))

;; * keytable

(defn- keytable-element-label-from-value [v]
  (cond
    (string? v) v
    (keyword? v) (name v)
    :else (str v)))

(defn- enrich-keytable-element [element]
  (-> element
      (assoc-if-missing :label (keytable-element-label-from-value (-> element :value)))))

(defn keytable [elements]
  (->> elements
       (map (fn [element]
              (if (map? element)
                element
                {:value element
                 :label (keytable-element-label-from-value element)})))
       (map enrich-keytable-element)
       (reduce (fn [m element]
                 (assoc m
                        (-> element :value)
                        element))
               (ordered.map/ordered-map))))

;; * functions

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

;; * strings


(defn string-trim [s]
  (when s (str/trim s)))

(def non-blank-string u/non-blank-string)

(defn non-blank-string-trimmed [s]
  (when s
    (-> s str str/trim non-blank-string)))

(defn string-uppercase-first-letter [s]
  (if (-> s count (< 1))
    s
    (str (-> s (subs 0 1) str/upper-case)
         (-> s (subs 1)))))

(defn split-lines [s]
  (when s
    (when-not (str/blank? s)
      (str/split-lines s))))

(defn string-pad-left [s min-len padding]
  (when s
    (let [s       (if (string? s) s (str s))
          padding (if (string? padding) padding (str padding))]
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

(defn string-includes-any? [s substrs]
  (->> substrs
       (filter #(str/includes? s %))
       first
       boolean))

(defn string-includes-all? [s substrs]
  (->> substrs
       (remove #(str/includes? s %))
       empty?))

(defn search [searchtext candidates candidate-values min-chars]
  (let [words (->> (str/split searchtext #"\s")
                   (map str/trim))
        shortest-word-len (->> words
                               (map count)
                               (apply clojure.core/max))]
    (when (-> shortest-word-len (>= min-chars))
      (let [words (->> words
                       (map str/lower-case))
            matches? (fn [candidate]
                       (->> (candidate-values candidate)
                            (remove nil?)
                            (map str/lower-case)
                            (filter (fn [value]
                                      (string-includes-all? value words)))
                            first
                            boolean))]
        (->> candidates
             (filter matches?))))))

;; * date and time

(def current-time-millis u/current-time-millis)

;; https://www.juxt.land/tick/docs/index.html

#?(:cljs
   (defn ->instant
     "Coerces `v` to `tick/instant`."
     [v]
     (when v
       (cond
         (time/instant? v) v
         (instance? js/Date v) (-> v time/instant)
         (time/date-time? v) (-> v time/instant)
         (string? v) (time/instant v)
         (number? v) (-> v time/instant)

         (and (map? v)
              (-> v :_seconds)
              (-> v :_nanoseconds))
         (time/instant (+ (-> v :_seconds (* 1000))
                          (-> v :_nanoseconds (/ 1000))))

         (and (-> ^js v .-seconds)
              (-> ^js v .-nanoseconds))
         (time/instant (+ (-> ^js v .-seconds (* 1000))
                          (-> ^js v .-nanoseconds (/ 1000))))

         :else (throw (ex-info (str "Unsupported time instant format: '" v "' of type '" (type v) "'")
                               {:value v
                                :type (type v)}))))))

(comment
  "js.Date" (->instant (js/Date. "2020-01-01")) := (time/instant (js/Date. "2020-01-01"))
  "millis" (->instant 1577870520000) := (time/instant 1577870520000)
  "string" (->instant "2020-01-01T10:00:00") := (time/instant "2020-01-01T10:00:00")
  "map" (->instant {:_seconds 1633078276, :_nanoseconds 210000000}))

#?(:cljs
   (defn ->date
     "Coerces `v` to `tick/date`."
     [v]
     (when v
       (cond
         (time/date? v) v
         (string? v) (time/date v)
         :else (-> v ->instant time/date)))))

(comment
  "nil" (->date nil) := nil
  "tick/date" (->date (time/date "2020-01-01")) := (time/date "2020-01-01")
  "js.Date" (->date (js/Date. "2020-01-01")) := (time/date "2020-01-01")
  "millis" (->date 1577870520000) := (time/date "2020-01-01")
  "string" (->date "2020-01-01") := (time/date "2020-01-01"))

#?(:cljs
   (defn ->time
     "Coerces `v` to `tick/time`."
     [v]
     (when v
       (cond
         (time/time? v) v
         (string? v) (time/time v)
         :else (-> v ->instant time/time)))))

(comment
  "nil" (->time nil) := nil
  "tick/time " (->time (time/time "12:23")) := (time/time "12:23")
  "js.Date" (->time (js/Date. "2020-01-01T12:23")) := (time/time "12:23")
  "millis" (->time 1577870520000) := (time/time "10:22")
  "string" (->time "12:23") := (time/time "12:23"))

;; (defn ->zoned-time [tz v]
;;   (when (and tz v)
;;     (cond
;;       (tick/instant? v) (-> v tick/date-time (tick/in tz))
;;       :else ":-("
;;       (tick/time? v) v
;;       (string? v) (tick/time v)
;;       :else (-> v ->instant tick/time))))

(comment
  (time/zone "Europe/Berlin")
  (time/zoned-date-time)
  (-> (time/instant) time/date-time (time/in "Europe/Berlin") time/time)
  ;; (->zoned-time "Europe/Berlin" (tick/instant))
  (js/Date.))

#?(:cljs
   (defn millis [thing]
     (cond
       (nil? thing)              nil
       (number? thing)           thing
       (instance? js/Date thing) (-> thing .getTime)
       (time/date-time? thing) (js/Date.parse (time/inst thing))
       (time/zoned-date-time? thing) (js/Date.parse (time/inst thing))
       :else                     (js/Date.parse thing))))

(comment
  (js/Date.)
  (js/Date. (millis (time/instant)))
  (js/Date. (millis (time/date-time)))
  (js/Date. (millis (time/zoned-date-time (time/date-time))))
  (js/Date. (millis "2020-01-01"))
  (js/Date. (millis 1577870520000))
  (js/Date. (millis (js/Date.))))

(def millis-in-second 1000)
(def millis-in-minute (* millis-in-second 60))
(def millis-in-hour (* millis-in-minute 60))
(def millis-in-day (* millis-in-hour 24))

(defn seconds->millis [seconds]
  (when seconds
    (* seconds millis-in-second)))

(defn minutes->millis [minutes]
  (when minutes
    (* minutes millis-in-minute)))

(defn hours->millis [hours]
  (when hours
    (* hours millis-in-hour)))

(defn days->millis [days]
  (when days
    (* days millis-in-day)))

(defn millis->days [millis]
  (when millis
    (quot millis millis-in-day)))

(defn millis->minutes [millis]
  (when millis
    (quot millis millis-in-minute)))

(defn millis->seconds [millis]
  (when millis
    (quot millis millis-in-second)))

(defn millis->hours-minutes [millis]
  (when millis
    (if (neg? millis)
      (str "-" (millis->hours-minutes (- 0 millis)))
      (let [hours (quot millis millis-in-hour)
            millis (rem millis millis-in-hour)
            minutes (quot millis millis-in-minute)]
        (str hours ":" (string-pad-left minutes 2 "0"))))))

(comment
  (millis->hours-minutes 1910000)
  (millis->hours-minutes 110000))

(defn timespans-overlapping? [a-start a-end b-start b-end]
  (not
   (or (<= a-end b-start)               ; a is before b / a ends before b start
       (<= b-end a-start)               ; b is before a / b ends before a starts
       )))

#?(:cljs
   (defn ->js-date [date-string]
     (when date-string
       (cond

         ;; already js/Date
         (instance? js/Date date-string)
         date-string

         :else (-> date-string millis js/Date.)

         ;; :else (js/Date. (js/Date.parse date-string))
         ))))
(comment
  (->js-date (time/instant))
  (->js-date "2020-01-01 10:22")
  (->js-date "2020-01-01"))

#?(:cljs
   (defn timestamp--now []
     (js/Date.)))

#?(:cljs
   (defn millis--now []
     (-> (js/Date.) .getTime)))

#?(:cljs
   (defn date [date-or-string]
     (when date-or-string
       (let [ts (->js-date date-or-string)]
         (str (-> ts .getFullYear)
              "-"
              (-> ts .getMonth inc (string-pad-left 2 "0"))
              "-"
              (-> ts .getDate (string-pad-left 2 "0")))))))

(comment
  (->js-date "2020-01-01 10:22")
  (date "2020-01-01 10:22"))

#?(:cljs
   (defn date-today []
     (date (js/Date.))))

(comment
  (date-today))

#?(:cljs
   (defn date-same-day? [date-a date-b]
     (let [date-a (->js-date date-a)
           date-b (->js-date date-b)]
       (and (= (-> date-a .getDate)   (-> date-b .getDate))
            (= (-> date-a .getMonth) (-> date-b .getMonth))
            (= (-> date-a .getFullYear)  (-> date-b .getFullYear))))))

(comment
  (date-same-day? (js/Date.) (js/Date.))
  (date-same-day? "2020-01-01" "2020-01-01")
  (date-same-day? "2020-01-01" "2020-01-02"))

#?(:cljs
   (defn date-before? [date test-date]
     (when date
       (let [date (->js-date date)
             test-date (->js-date test-date)]

         (cond
           (< (-> date .getFullYear) (-> test-date .getFullYear)) true
           (> (-> date .getFullYear) (-> test-date .getFullYear)) false
           :else (cond
                   (< (-> date .getMonth) (-> test-date .getMonth)) true
                   (> (-> date .getMonth) (-> test-date .getMonth)) false
                   :else (cond
                           (< (-> date .getDate) (-> test-date .getDate)) true
                           (> (-> date .getDate) (-> test-date .getDate)) false
                           :else false)))))))

(comment
  (date-before? "2020-01-01" "2020-01-02")
  (date-before? "2020-01-01" "2020-02-01")
  (date-before? "2020-01-01" "2021-01-01")
  (date-before? "2020-01-01" "2020-01-01")
  (date-before? "2020-01-02" "2020-01-01")
  (date-before? "2020-02-01" "2020-01-01")
  (date-before? "2022-01-01" "2020-01-01"))

#?(:cljs
   (defn date-past? [date]
     (when date
       (date-before? date (date-today)))))

(comment
  (date-today)
  (date-past? (js/Date.))
  (date-past? (date-today))
  (date-past? "2021-04-01")
  (date-past? "2021-04-02")
  (date-past? "2021-04-03"))

#?(:cljs
   (defn time-of-date
     ([date-or-string]
      (time-of-date date-or-string false false))
     ([date-or-string seconds?]
      (time-of-date date-or-string seconds? false))
     ([date-or-string seconds? milliseconds?]
      (when date-or-string
        (let [ts (->js-date date-or-string)]
          (str (-> ts .getHours (string-pad-left 2 "0"))
               ":"
               (-> ts .getMinutes (string-pad-left 2 "0"))
               (when seconds?
                 (str ":" (-> ts .getSeconds (string-pad-left 2 "0"))))
               (when (and seconds? milliseconds?)
                 (str ":" (-> ts .getMilliseconds (string-pad-left 3 "0"))))))))))

(comment
  (time-of-date (js/Date.))
  (time-of-date (js/Date.) true)
  (time-of-date (js/Date.) true true))

;; * promises

#?(:cljs
   (defn promise> [f-with-resolve-and-reject]
     (js/Promise. f-with-resolve-and-reject)))

#?(:cljs
   (defn resolve> [result]
     (js/Promise.resolve result)))

#?(:cljs
   (defn reject> [result]
     (js/Promise.reject result)))

#?(:cljs
   (defn no-op> []
     (js/Promise.resolve nil)))

#?(:cljs
   (defn as>
     "Converge `thing` to js/Promise."
     [thing]
     (cond
       (nil? thing)
       (no-op>)

       (instance? js/Promise thing)
       thing

       (fn? thing)
       (promise> (fn [resolve _reject]
                   (resolve (thing))))

       :else
       (resolve> thing))))

(comment
  (tap> (as> :boo))
  (tap> (as> (fn [] "executed")))
  (-> (as> (fn [] "executed"))
      (.then (fn [result]
               (prn "result:" result)))))

#?(:cljs
   (defn- as-promises-vector [promises-or-lists-of-promises]
     (reduce (fn [promises promise-or-list]
               (cond
                 (nil? promise-or-list)
                 promises

                 (instance? js/Promise promise-or-list)
                 (conj promises promise-or-list)

                 :else
                 (->> promise-or-list
                      (map as>)
                      (into promises))))
             [] promises-or-lists-of-promises)))

#?(:cljs
   (defn all> [& promises-or-lists-of-promises]
     (let [promises (as-promises-vector promises-or-lists-of-promises)]
       (-> (js/Promise.allSettled promises)
           (.then (fn [results]
                    (->> results
                         #_(map (fn [^js result]
                                  (when-let [error (-> result .-reason)]
                                    (js/console.error "Error while spark.utils/all>" error))
                                  result))
                         (map (fn [^js result]
                                (when-let [error (-> result .-reason)]
                                  (throw error)
                                  #_(throw (ex-info (str "A promise in all> was rejected: "
                                                         error)
                                                    {:error error
                                                     :result result}
                                                    error)))
                                (-> result .-value)))
                         vec)))))))

(comment
  (tap> (all> [(js/Promise.resolve "a")
               (js/Promise.resolve "b")]))

  (tap> (all> [(js/Promise.resolve "a")
               (js/Promise.reject "boom!")]))

  (all>
   (promise> (fn [resolve]
               (js/console.log "#1")
               (resolve "1")))
   (promise> (fn [resolve]
               (js/console.log "#2")
               (resolve "2")))))

#?(:cljs
   (defn- next-promise> [promises results]
     (if-let [next-promise (first promises)]
       (-> next-promise
           (.then (fn [result]
                    (next-promise> (rest promises) (conj results result)))))
       (as> results))))

#?(:cljs
   (defn all-in-sequence> [& promises-or-lists-of-promises]
     (next-promise> (as-promises-vector promises-or-lists-of-promises) [])))

(comment
  (tap>
   (all-in-sequence>
    (as> "#1")
    (as> "#2"))))

#?(:cljs
   (defn sleep> [millis]
     (js/Promise.
      (fn [resolve _]
        (js/setTimeout #(resolve millis)
                       millis)))))
#?(:cljs
   (def later> u/later>))

#?(:cljs
   (defn => [promise & thens]
     (reduce (fn [promise then]
               (-> promise
                   (.then (fn [result]
                            (as> (then result))))))
             (as> promise) thens)))

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

#?(:cljs
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
                   reject))))))

#?(:cljs
   (defn p-transformed
     "Wraps promise function `f>` with `transform` function.

  Use this if you have a promise function which value needs to be transformed."
     [f> transform]
     (fn [& args]
       (transform> (apply f> args) transform))))

#?(:cljs
   (defn chain-promise-fns> [input-value fns]
     (let [fns (remove nil? fns)]
       (if-let [fn> (first fns)]
         (-> (fn> input-value)
             (.then #(chain-promise-fns> % (rest fns))))
         (js/Promise.resolve input-value)))))

;; * deprecations

#?(:cljs
   (defn log-deprecated [info]
     (js/console.error "DEPRECATED" (js/Error info))))

;; * CSV

(defn csv-field [v]
  (if v
    (str "\""
         (-> v
             str
             (.replace "\\" "\\\\")
             (.replace "\"" "\\\""))
         "\"")
    ""))

(defn csv-record [fields]
  (->> fields
       (map csv-field)
       (str/join ", ")))

(defn csv-table [records]
  (->> records
       (map csv-record)
       (str/join "\n")))

;; * Links / href

(defn ->href [link]
  (when link
    (cond

      (or (-> link (.startsWith "http:"))
          (-> link (.startsWith "https:"))
          (-> link (.startsWith "mailto:"))
          (-> link (.startsWith "tel:")))
      link

      :else
      (str "http://" link))))

;; * malli

(defn malli-explain->user-message [explain schema]
  (when explain
    (str "Value does not match schema: "
         (try
           (malli-error/humanize explain)
           (catch #?(:cljs :default :clj Exception) ex
             (throw (ex-info "Malli schema error humanization failed."
                             {:schema  schema
                              :explain explain}
                             ex)))))))

(defn malli-explain [schema value]
  (try
    (malli/explain schema value)
    (catch #?(:cljs :default :clj Exception) ex
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

;;; texts

(def LOCAL_TEXTS u/LOCAL_TEXTS)
(defmacro text [k de-text & [arg-map]] `(u/text ~k ~de-text ~arg-map))

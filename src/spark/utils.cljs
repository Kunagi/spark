(ns spark.utils
  (:require
   [clojure.spec.alpha :as s]
   [cljs.pprint :refer [pprint]]

   [goog.object :as gobj]

   [malli.core :as malli]
   [malli.error :as malli-error]))


;;; maps

(defn deep-merge
  [& maps]
  (apply merge-with (fn [& args]
                      (if (every? map? args)
                        (apply deep-merge args)
                        (last args)))
         maps))


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

;;; edn

(defn ->edn [data]
  (with-out-str (pprint data)))

;;; promises

(defn no-op> []
  (js/Promise.resolve nil))

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
  (str "Value does not match schema: "
       (try
         (malli-error/humanize explain)
         (catch :default ex
           (throw (ex-info "Malli schema error humanization failed."
                           {:schema schema
                            :explain explain}
                           ex))))))

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

    (or (nil? data) (string? data) (number? data))
    (js->clj data)

    (instance? js/firebase.firestore.Timestamp data)
    (-> data .toDate)

    ^boolean (js/Array.isArray data)
    (mapv #(conform-js-data % nil) data)

    :else
    (case (first schema)
      :map-of
      (reduce (fn [m js-key]
                (let [k js-key
                      v (gobj/get data js-key)]
                  (assoc m k (conform-js-data v (malli-map-field-schema-by-id schema k)))))
              {} (js/Object.keys data))
      (reduce (fn [m js-key]
                (let [k (keyword js-key)
                      v (gobj/get data js-key)]
                  (assoc m k (conform-js-data v (malli-map-field-schema-by-id schema k)))))
              {} (js/Object.keys data)))))

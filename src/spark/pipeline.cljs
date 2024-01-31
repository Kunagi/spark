(ns spark.pipeline
  "Processing a pipeline of synchronous or asynchronous functions which operate
  on a common `context` map."

  (:require
   [spark.utils :as u]
   [spark.logging :refer [log]]))

(declare continue>)

(defn- error [ex context op]
  (ex-info "Pipeline Operation failed."
           {:context context
            :op op}
           ex))

(defn- process-op-result [result context op]
  (cond

    (= result :abort)
    (-> context
        (assoc ::ops []))

    :else
    (do
      (when-not (-> result ::ops)
        (throw (ex-info "Pipeline Operation returned invalid result."
                        {:context context
                         :op op
                         :returned-value result})))
      result)))

(defn op-impl [op]
  (cond

    (= op :debug)
    (fn debug [context] (assoc context ::debug true))

    (= op :log)
    (fn [context]
      (log ::log :context context)
      context)

    (= op :abort)
    (fn abort [context]
      (assoc context ::ops []))

    ;; update
    (and (vector? op) (-> op first (= :update)))
    (fn with [context]
      (let [[_ k f & args] op
            v (get context k)
            result (apply f (into [v] args))]
        (if (u/promise? result)
          (-> result
              (.then (fn [result]
                       (assoc context k result))))
          (assoc context k result))
        ))

    ;; assoc
    (and (vector? op) (-> op first (= :assoc)))
    (fn with [context]
      (let [[_ k f] op
            result (f context)]
        (if (u/promise? result)
          (-> result
              (.then (fn [result]
                       (assoc context k result))))
          (assoc context k result))
        ))

    ;; just functions
    (and (vector? op) (-> op first fn?))
    (fn [context]
      (apply (first op) (into [context] (rest op))))

    :else
    (throw (ex-info "Invalid Operation"
                    {:op op}))))

(defn- exec-op> [context op]
  (try
    (let [op (if (fn? op)
               op
               (op-impl op))
          result (op context)]
      (if (u/promise? result)
        (-> result
            (.then #(-> %
                        (process-op-result context op)
                        continue>))
            (.catch #(js/Promise.reject (error % context op))))
        (-> result
            (process-op-result context op)
            continue>)))
    (catch :default ex
      (js/Promise.reject (error ex context op)))))


(defn- cleanup [context]
  (-> context
      (dissoc ::ops ::debug)))

(defn- continue> [context]
  (let [ops (-> context ::ops)
        op (first ops)]
    (when (and op (-> context ::debug))
      (log ::continue>
           :op op
           :context context))
    (if-not op
      (js/Promise.resolve (cleanup context))
      (exec-op> (update context ::ops rest) op))))

(defn => [context & ops]
  (if-let [existing-ops (-> context ::ops)]
    ;; child pipeline
    (-> context
        (assoc ::ops ops)
        continue>
        (.then (fn [context]
                 (assoc context ::ops existing-ops))))
     ;; root pipeline
    (-> context
        (assoc ::ops ops)
        continue>)))

(comment
  (do

    (js/console.log "- - - - - - - - - - -")

    (defn op1 [context]
      (update context :log conj "op1"))

    (defn op2> [context]
      (js/Promise.
       (fn [resolve _reject]
         (resolve (update context :log conj "op2>")))))

    (defn op3 [context param]
      (update context :log conj param))

    (defn op4 [log param]
      (conj log param))

    (defn op5> [log]
      (js/Promise.resolve
       (conj log "op5>")))

    (defn fail [context]
      (throw (ex-info "fail!" {:failure-info "none"})))

    (defn fail> [context]
      (js/Promise.
       (fn [resolve reject]
         (throw (ex-info "fail!" {:failure-info "none"})))))

    (defn reject> [context]
      (js/Promise.reject "reject!"))

    (defn defect [context]
      {:illegal :context})

    (defn defect> [context]
      (js/Promise.resolve
       {:illegal :context}))

    (defn sub-pipeline> [context]
      (=> context
          (fn [context] (update context :log conj "sub-pipeline>"))))

    (defn abort [context]
      :abort)

    (defn abort> [context]
      (js/Promise.resolve :abort))

    (defn show-form> [form extra-param]
      (js/Promise.resolve {:name extra-param}))

    (-> (=> {:log []}
            :debug
            op1
            [:assoc :values #(show-form> (-> % :log) "Witek")]
            #_abort>
            #_abort
            #_:abort
            #_op2>
            #_[op3 "op3"]
            #_[:update :log conj "op4"]
            #_[:update :log op5>]
            #_sub-pipeline>
            #_:log
            #_defect
            #_defect>
            #_fail
            #_fail>
            #_reject>
            [op3 "op-last"])
        (.then #(js/console.log "SUCCESS:" %))
        (.catch #(js/console.log "FAILURE:" %)))))

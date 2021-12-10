(ns spark.runtime
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.repository :as repository]))

(def $Query
  [:map
   [:path any?]])

(def $Command
  [:map
   [:args {:optional true} map?]
   [:f any?]])

(defn validate-command [command]
  (when-let [explain (u/malli-explain $Command command)]
    (throw (ex-info (str  "Invalid command: "
                          (u/malli-explain->user-message explain $Command))
                    {:command command
                     :explain explain}))))

(defn- context-arg->malli [arg]
  (if-let [malli (get-in arg [1 :$])]
    (assoc arg 1 malli)
    (if-let [malli (get-in arg [2 :$])]
      (assoc arg 2 malli)
      arg)))

(defn context-args->malli [args]
  (into [:map] (map context-arg->malli args)))

(defn validate-command-context [command context]
  (when-let [args (-> command :context-args)]
    ;; (log ::validate-command-context
    ;;      :args args)
    (when-let [explain (u/malli-explain (context-args->malli args) context)]
      (throw (ex-info (str  "Invalid command context: "
                            (u/malli-explain->user-message explain (context-args->malli args)))
                      {:command command
                       :context context
                       :explain explain}))))
  (when (-> command :args)
    (js/console.error "Command has deprecated :args property:" command)
    (doseq [[k schema] (-> command :args)]
      (let [v (get context k)]
        (when-not v
          (throw (ex-info (str  "Missing `" k "` in context.")
                          {:command command
                           :context-key k})))
        (when-let [explain (u/malli-explain schema v)]
          (throw (ex-info (str  "Invalid `" k "` in context: "
                                (u/malli-explain->user-message explain schema))
                          {:command command
                           :context-key k
                           :context-value v
                           :explain explain})))))))

(defn post-process-query-result [query context result]
  (if-let [f (-> query spark/query-process)]
    (f result context)
    result))

(defn execute-query>
  [query context]
  (log ::execute-query>
       :query query
       :context context)

  (-> (if-let [path (-> query :path (u/fn->value context))]
        (repository/query> path)
        (repository/query-union> (-> query :paths (u/fn->value context))))
      (.then (fn [result]
               (u/resolve> (post-process-query-result query context result))))))

(defn execute-command>
  [command context]
  (js/console.log "[DEBUG] runtime/execute-command>")
  (log ::execute-command>
       :command command
       :context context)
  ;; (validate-command command)
  ;; (validate-command-context command context)
  (js/console.log "[DEBUG] runtime/execute-command>--after-validation")
  (let [f (get command :f)
        result (f context)]
    (js/console.log "[DEBUG] runtime/execute-command>--after-f")
    (when-not (instance? js/Promise result)
      (log ::execute-command>--WARN-result-is-not-a-promise
           :result result))
    (u/as> result)))

(defn report-error [error]
  (js/console.log error)
  (when-not ^boolean js/goog.DEBUG
    (when (exists? js/firebase.analytics)
      (-> js/firebase
          .analytics
          (.logEvent "exception" #js {:description error})))))

(ns spark.runtime
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.repository :as repository]
    ))


(def $Query
  [:map
   [:path any?]])

(def $Command
  [:map
   [:args {:optional true} map?]
   [:f any?]])

(def $Effect
  [:vector any?])

(def $Effects
  [:vector $Effect])


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
    (log ::validate-command-context
         :args args)
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


(defn validate-effects [command effects]
  (when-let [explain (u/malli-explain $Effects effects)]
    (throw (ex-info (str  "Invalid command effects: "
                          (u/malli-explain->user-message explain $Effects))
                    {:command command
                     :effects effects
                     :explain explain}))))


(defmulti reify-effect> (fn [effect] (first effect)))


(defn reify-effects> [effects]
  (log ::reify-effects>
       :effects effects)
  (js/Promise.all
   (mapv reify-effect> effects)))

(defn post-process-query-result [query context result]
  (when-let [f (-> query spark/query-process)]
     (f result context)))

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
  (log ::execute-command>
       :command command
       :context context)
  (js/Promise.
   (fn [resolve _reject]
     (validate-command command)
     (validate-command-context command context)
     (let [f (get command :f)
           result (f context)]
       (if (instance? js/Promise result)
         (-> result
             (.then resolve))
         (let [effects result]
           (validate-effects command effects)
           (-> (reify-effects> effects)
               (.then resolve))))))))


(defn report-error [error]
  (js/console.log error)
  (when-not ^boolean js/goog.DEBUG
    (when (exists? js/firebase.analytics)
      (-> js/firebase
          .analytics
          (.logEvent "exception" #js {:description error})))))

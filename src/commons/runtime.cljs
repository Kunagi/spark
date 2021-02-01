(ns commons.runtime
  (:require
   [malli.core :as m]
   [malli.error :as me]
   [commons.logging :refer [log]]
   [commons.models :as models]))


(def $Command
  [:map
   [:args {:optional true} map?]
   [:f any?]])

(def $Effect
  [:vector any?])

(def $Effects
  [:vector $Effect])


(defn validate-command [command]
  (when-let [explain (m/explain $Command command)]
    (throw (ex-info (str  "Invalid command: "
                          (me/humanize explain))
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
    (when-let [explain (m/explain (context-args->malli args) context)]
      (throw (ex-info (str  "Invalid context: "
                            (me/humanize explain))
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
        (when-let [explain (m/explain schema v)]
          (throw (ex-info (str  "Invalid `" k "` in context: "
                                (me/humanize explain))
                          {:command command
                           :context-key k
                           :context-value v
                           :explain explain})))))))


(defn validate-effects [command effects]
  (when-let [explain (m/explain $Effects effects)]
    (throw (ex-info (str  "Invalid command effects: "
                          (me/humanize explain))
                    {:command command
                     :effects effects
                     :explain explain}))))


(defmulti reify-effect> (fn [effect] (first effect)))

;;; effect implementations


;;;

(defn reify-effects> [effects]
  (log ::reify-effects>
       :effects effects)
  (js/Promise.all
   (mapv reify-effect> effects)))


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
           effects (f context)]
       (validate-effects command effects)
       (-> (reify-effects> effects)
           (.then resolve))))))


(defn report-error [error]
  (when-not ^boolean js/goog.DEBUG
    (when (exists? js/firebase.analytics)
      (-> js/firebase
          .analytics
          (.logEvent "exception" #js {:description error})))))

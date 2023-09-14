(ns spark.gcf.tasks
  (:require
   [spark.utils :as u]))

(defonce TASKS (atom {}))

(defn register-task [{:keys [id f] :as task}]
  (u/assert (string? id))
  (u/assert (fn? f))
  (swap! TASKS assoc id task))

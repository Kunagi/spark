(ns spark.ui.showcase
  (:require
   [spark.logging :refer [log]]))

(defonce SHOWCASES (atom {}))

(defn reg-showcase [id showcase]
  (log ::reg-showcase
       :id id)
  (swap! SHOWCASES assoc id (assoc showcase :showcase/id id)))

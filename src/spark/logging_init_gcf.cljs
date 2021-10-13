(ns spark.logging-init-gcf
  (:require
   ["firebase-functions" :as functions]
   [spark.logging :as logging]))

(def ^js logger (-> functions .-logger))

(defn writer [event-namespace event-name event-data]
  (if goog.DEBUG
    (logging/console-writer js/console event-namespace event-name event-data)
    (try
      (-> logger
          (.debug event-namespace event-name (clj->js event-data)))
      (catch :default ex
        (-> logger (.error "Failed to log" event-namespace event-name event-data ex))))))

(reset! logging/WRITER writer)

;; (reset! logging/WRITER (partial logging/console-writer
;;                           js/console
;;                           ))

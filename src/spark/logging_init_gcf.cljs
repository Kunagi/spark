(ns spark.logging-init-gcf
  (:require
   ["firebase-functions" :as functions]
   [spark.logging :as logging]))


(reset! logging/WRITER (partial logging/console-writer
                          ;; (-> functions .-logger)
                          js/console
                          ))

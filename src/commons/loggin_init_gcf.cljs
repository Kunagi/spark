(ns commons.loggin-init-gcf
  (:require
   ["firebase-functions" :as functions]
   [commons.logging :as logging]))


(reset! logging/WRITER (partial logging/console-writer (-> functions .-logger)))

(ns projekt.sprint
  (:require
   [spark.core :as spard :refer [def-field def-subdoc]]
   ))


(def-field Entwickler
  [:string
   {:label "Entiwickler"}])

(def-field Datum-abgeschlossen
  [:string
   {:label "Abgeschlossen am"
    :type :date}])


(def-subdoc Sprint
  [{}])


(defn id [this]
  (-> this :id))

(defn datum-abgeschlossen [this]
  (-> this :datum-abgeschlossen))

(defn entwickler [this]
  (-> this :entwickler))

(defn closed? [this]
  (or (-> this datum-abgeschlossen boolean)
      (-> this id (> 99999))))

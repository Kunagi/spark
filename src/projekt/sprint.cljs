(ns projekt.sprint
  (:require
   [spark.core :as spard :refer [def-field def-subdoc]]
   ))


(def-field Entwickler
  [:string
   {:label "Entiwickler"}])

(def-field Datum-Beginn
  [:string
   {:label "Beginn am"
    :type :date}])

(def-field Datum-Abgeschlossen
  [:string
   {:label "Abgeschlossen am"
    :type :date}])

(def-field Tagesleistung
  [:int
   {:label "Leistung pro Tag (in Stunden)"
    :type "number"}])

(def-subdoc Sprint
  [{}])


(defn id [this]
  (-> this :id))

(defn datum-abgeschlossen [this]
  (-> this :datum-abgeschlossen))

(defn datum-beginn [this]
  (-> this :datum-beginn))

(defn tagesleistung [this]
  (-> this :tagesleistung))

(defn entwickler [this]
  (-> this :entwickler))

(defn closed? [this]
  (or (-> this datum-abgeschlossen boolean)
      (-> this id (> 99999))))

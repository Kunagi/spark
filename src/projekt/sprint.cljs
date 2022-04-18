(ns projekt.sprint
  (:require
   [tick.core :as tick]
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

(defn arbeitstage-ab [this instant]
  (when-let [datum (-> this datum-beginn)]
    (let [start-date (tick/max (tick/date datum) (tick/date instant))]
      (->> (range 30)
           (map (fn [i]
                  (tick/>> start-date i)))
           (remove #(let [weekday (-> % tick/day-of-week)]
                      (or (= weekday tick/SATURDAY)
                          (= weekday tick/SUNDAY)))))
      )))

(comment
  (-> (tick/date) (tick/>> 1))
  (-> (tick/date) tick/day-of-week)
  (tick/>> (tick/instant)
           (tick/new-duration 1 :days)))

(defn tagesleistung [this]
  (-> this :tagesleistung))

(defn entwickler [this]
  (-> this :entwickler))

(defn closed? [this]
  (or (-> this datum-abgeschlossen boolean)
      (-> this id (> 99999))))

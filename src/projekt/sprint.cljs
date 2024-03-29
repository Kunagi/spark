(ns projekt.sprint
  (:require
   [spark.core :as spard :refer [def-field def-subdoc]]
   [spark.time :as time]
   [spark.local :as local]))

(def-field Entwickler
  [:string
   {:label "Entiwickler"}])

(def-field Datum-Beginn
  [:string
   {:label "Beginn am"
    :type :date}])

(def-field Datum-Ende
  [:string
   {:label "Ende, geplant am"
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
  [{}
   [:gesamtleistung-pro-tag
    {:optional true}
    [:map-of
     :string ; Datum
     :int ; Anzahl Stunden
     ]]])


(defn id [this]
  (-> this :id))

(defn gesamtleistung-am [this datum]
  (-> this :gesamtleistung-pro-tag
      (get (str datum))))

(defn gesamtleistung-vor [this datum]
  (->> this :gesamtleistung-pro-tag
       (filter #(-> % first time/date (time/< datum)))
       (sort-by first)
       reverse
       first
       second))

(defn datum-abgeschlossen [this]
  (-> this :datum-abgeschlossen))

(defn datum-beginn [this]
  (-> this :datum-beginn))

(defn datum-ende [this]
  (-> this :datum-ende))

(defn arbeitstage-ab [this instant]
  (when-let [datum (-> this datum-beginn)]
    (let [start-date (time/max (time/date datum) (time/date instant))]
      (->> (range 360)
           (map (fn [i]
                  (time/>> start-date i)))
           (remove #(let [weekday (-> % time/day-of-week)]
                      (or (= weekday time/SATURDAY)
                          (= weekday time/SUNDAY)))))
      )))

(comment
  (-> (time/date) (time/>> 1))
  (-> (time/date) time/day-of-week)
  (time/>> (time/instant)
           (time/new-duration 1 :days)))

(defn tagesleistung [this]
  (-> this :tagesleistung))

(defn entwickler [this]
  (-> this :entwickler))

(defn closed? [this]
  (or (-> this datum-abgeschlossen boolean)
      (-> this id (> 99999))))

(defn long-label [this]
  (str "Sprint #" (-> this id)
       (when-let [s (-> this entwickler)]
         (str " | " s))
       (when-let [s (-> this datum-abgeschlossen)]
         (str " | " (-> s local/format-date)))
       (when (-> this id (= "99"))
         " | später")
       (when (-> this id (= "99999"))
         " | zurückgestellt")))

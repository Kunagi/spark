(ns projekt.story
  (:require
   [clojure.string :as str]
   [spark.core :as spark :refer [def-field def-subdoc]]))

(def-field Bez
  [:string
   {:label "Bezeichnung"}])

(def-field Beschreibung
  [:string
   {:label "Beschreibung"
    :rows 3}])

(def-field Voraussetzungen
  [:string
   {:label "Voraussetzungen"}])

(def-field Tasks
  [:string
   {:label "ToDo's"
    :rows 12}])

(def-field Klaerungsbedarf
  [:string
   {:label "Klärungsbedarf (für Product Owner)"
    :rows 8}])

(def-field Feature-id
  [:string
   {:label "Feature ID"
    :default-value "zzzzz"}])

(def-field Sprint-id
  [:string
   {:label "Sprint Nummer"
    :default-value "99999"}])

(def-field Aufwand
  [:string
   {:label "Aufwand (in Stunden)"
    :type "number"
    :input-props {:min 0 :step 1}
    :plusminus-adornments true}])

(def-field Aufwandschaetzung
  [:string
   {:label "Aufwandschätzung (in Stunden)"
    :type "number"
    :input-props {:min 0 :step 4}}])

(def-field Prio
  [:int
   {:label "Priorität"
    :type :number}])

(def-subdoc Story
  [{}
   [:prio {:optional true} Prio]])

(defn num [story]
  (-> story :id int))

(defn prio [this]
  (-> this :prio))

(defn sort-value [this]
  [(or (-> this prio)
       99999)
   (-> this num)])

(defn parse-task [task]
  (let [prefix (-> task (.substring 0 1))
        bez (-> task (.substring 2))
        done? (= prefix "x")]
    {:bez bez
     :prefix prefix
     :done? done?}))

(defn parse-tasks [story]
  (when-let [tasks (-> story :tasks)]
    (->> tasks
         str/split-lines
         (map parse-task))))

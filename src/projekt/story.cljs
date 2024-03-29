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

(def-field Hindernis
  [:string
   {:label "Hindernisse"
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

(defn bez [story]
  (-> story :bez))

(defn beschreibung [story]
  (-> story :beschreibung))

(defn tasks [story]
  (-> story :tasks))

(defn ts-completed [this]
  (-> this :ts-completed))

(defn klaerungsbedarf [story]
  (-> story :klaerungsbedarf))

(defn prio [this]
  (-> this :prio))

(defn hindernis [this]
  (-> this :hindernis))

(defn sprint-id [this]
  (-> this :sprint-id))

(defn aufwand [this]
  (-> this :aufwand))

(defn aufwandschaetzung [this]
  (-> this :aufwandschaetzung))

(defn parse-task [task]
  (let [prefix (-> task (.substring 0 1))
        bez (-> task (.substring 2))
        done? (= prefix "x")]
    {:bez bez
     :prefix prefix
     :done? done?}))

(defn parse-tasks-from-text [s]
  (when s
    (->> s
         str/split-lines
         (map parse-task))))

(def parse-tasks-from-text-memoized (memoize parse-tasks-from-text))

(defn parse-tasks [story]
  (-> story :tasks parse-tasks-from-text-memoized))

(defn completed? [this]
  (let [tasks (->> this
                   parse-tasks)]
    (if (empty? tasks)
      false
      (->> tasks
           (remove :done?)
           empty?))))

(defn sort-value [this]
  [(if (-> this completed?) 1 2)
   (or (-> this prio) 99999)
   (-> this num)])

(defn restaufwand [this]
  (if (-> this completed?)
    0
    (when-let [rest (-> this aufwandschaetzung)]
      (if-let [aufwand (-> this aufwand)]
        (- rest aufwand)
        rest))))

(defn matches-suchtext [this suchtext]
  (or
   (when-let [s (-> this bez)]
     (-> s str/trim str/lower-case (.indexOf suchtext) (>= 0)))
   (when-let [s (-> this beschreibung)]
     (-> s str/trim str/lower-case (.indexOf suchtext) (>= 0)))
   (when-let [s (-> this tasks)]
     (-> s str/trim str/lower-case (.indexOf suchtext) (>= 0)))
   (when-let [s (-> this klaerungsbedarf)]
     (-> s str/trim str/lower-case (.indexOf suchtext) (>= 0)))
   (when-let [n (-> this num)]
     (-> n str str/trim str/lower-case (.indexOf suchtext) (>= 0)))))

(ns projekt.projekt
  (:require
   [clojure.string :as str]
   [projekt.sprint :as sprint]
   [projekt.story :as story]
   [spark.core :as spark :refer [def-doc def-field def-test]]
   [spark.utils :as u]))

(def-field Storys
  [:map-of
   {:label "Storys"}
   :string story/Story])

(def-field Sprints
  [:map-of
   {:label "Sprints"}
   :string sprint/Sprint])

(defn sprints [this]
  (-> this :sprints vals))

(def-doc Projekt
  [{:firestore/collection "projekte"}
   [:storys {:optional true} Storys]
   [:sprints {:optional true} Sprints]
   [:developers [:set :string]]])

(def-test [Projekt]
  (u/assert-malli
   Projekt
   {:storys {"a" {:id "a"}}}))

(defn issues-attr-id [type]
  (-> type name (str "s") keyword))

(defn issues [projekt type]
  (when-let [v (get projekt (issues-attr-id type))]
    (str/split-lines v)))

(def issue-types-label
  {:bug "Fehler"
   :debt "Verbindlichkeiten"})

(defn feature-reihenfolge [this]
  (-> this :feature-reihenfolge))

(defn developers [this]
  (-> this :developers))

(defn developer-uid? [this uid]
  (-> this developers (contains? uid)))

(defn product-owner-uid? [projekt uid]
  (if-let [uids (-> projekt :product-owners)]
    (u/v-contains? uids uid)
    true))

(defn all-storys [this]
  (->> this
       :storys
       vals))

(defn storys [this]
  (->> this
       all-storys
       (remove :deleted)))

(defn next-story-id [this]
  (str
   (inc
    (reduce (fn [id story]
              (-> story
                  :id
                  js/parseInt
                  (max id)))
            0 (all-storys this)))))

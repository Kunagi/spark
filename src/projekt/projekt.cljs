(ns projekt.projekt
  (:require
   [clojure.string :as str]
   [spark.utils :as u]
   [spark.core :as spark :refer [def-doc def-test def-field]]

   [projekt.story :as story]
   [projekt.sprint :as sprint]
   ))

(def-field Storys
  [:map-of
   {:label "Storys"}
   :string story/Story])

(def-field Sprints
  [:map-of
   {:label "Sprints"}
   :string sprint/Sprint])

(def-doc Projekt
  [{:firestore/collection "singletons"}
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

(defn developers [this]
  (-> this :developers))

(defn developer-uid? [this uid]
  (-> this developers (contains? uid)))

(defn product-owner-uid? [projekt uid]
  (if-let [uids (-> projekt :product-owners)]
    (u/v-contains? uids uid)
    true))


(defn storys [this]
  (-> this :storys vals))


(defn next-story-id [this]
  (str
   (inc
    (reduce (fn [id story]
              (-> story
                  :id
                  js/parseInt
                  (max id)))
            0 (storys this)))))

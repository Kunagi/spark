(ns projekt.altprojekt
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

(def-doc Altprojekt
  [{:firestore/collection "singletons"}
   [:storys {:optional true} Storys]
   [:sprints {:optional true} Sprints]
   [:developers [:set :string]]])

(ns projekt.commands
  (:require

   [spark.logging :refer [log]]
   [spark.core :as spark :refer [def-cmd]]

   [projekt.projekt :as projekt]
   [projekt.story :as story]
   [projekt.sprint :as sprint ]
   ))


(def-cmd add-story
  {:label "Neue Story"
   :icon "add"

   :context-args [[:projekt projekt/Projekt]
                  [:sprint-id {:optional true} string?]
                  [:feature-id {:optional true} string?]]

   :form {:fields [story/Bez story/Beschreibung
                   story/Tasks
                   story/Voraussetzungen
                   story/Feature-id story/Sprint-id
                   story/Aufwand]}

   :f (fn [{:keys [projekt values]}]
        (let [story (-> values
                        (assoc :id (projekt/next-story-id projekt)))]
          [[:db/add-child projekt [:storys] story]]))})


(def-cmd update-story
  {:label "Story bearbeiten"

   :context-args [[:projekt projekt/Projekt
                   :story map?
                   :uid string?]]

   :form (fn [{:keys [projekt story uid]}]
           (let [fields (if (projekt/developer-uid? projekt uid)
                          [story/Bez story/Beschreibung
                           story/Tasks
                           story/Voraussetzungen
                           story/Klaerungsbedarf
                           story/Feature-id story/Sprint-id
                           story/Aufwand]
                          [story/Klaerungsbedarf])]
             {:fields fields
              :values story}))

   :f (fn [{:keys [projekt values]}]
        (log ::update-story
             :values values)
        [[:db/update-child projekt [:storys] (-> values :id) values]])})


(def-cmd update-sprint
  {:label "Sprint bearbeiten"
   :icon "edit"

    :context-args [[:projekt projekt/Projekt
                    :sprint sprint/Sprint]]

    :form (fn [{:keys [sprint]}]
            (let [fields [sprint/Entwickler
                          sprint/Datum-abgeschlossen] ]
              {:fields fields
               :values sprint}))

    :f (fn [{:keys [projekt values]}]
         [[:db/update-child projekt [:sprints] (-> values :id) values]])})

(ns projekt.storymap-page
  (:require

   ["@material-ui/core" :as mui]

   [spark.logging :refer [log]]
   [spark.utils :as u]

   [spark.ui :as ui :refer [def-page def-ui $ <>]]

   [projekt.core :as core]
   [projekt.story :as story]
   [projekt.sprint :as sprint]
   [projekt.projekt :as projekt]
   [projekt.commands :as commands]))

(def-ui Task [task]
  ($ :div
     {:style {:display :flex}}
     ($ :div
        {:class "material-icons"
         :style {:margin-right "4px"}}
        (if (-> task :done?)
          "done"
          "check_box_outline_blank"))
     ($ :div
        {:style {:margin-top "4px"}}
        (-> task :bez))))

(def-ui StoryCard [story]
  {:wrap-memo-props [story]}
  (log ::StoryCard--render
       :story (-> story story/num))
  (let [theme (ui/use-theme)]
    ($ mui/Card
       ($ ui/CommandCardArea
          {:command commands/update-story
           :context {:story story}}
          ($ mui/CardContent
             ($ ui/Stack
                ($ :div
                   {:style {:text-align "center"}}
                   ($ :span "#" (-> story :id)))
                ($ :div
                   {:style {:text-align "center"
                            :width "200px"
                            :font-weight "bold"}}
                   (-> story :bez))
                (when-let [beschreibung (-> story :beschreibung)]
                  ($ :div
                     {:style {:text-align "center"
                              :white-space "pre-wrap"}}
                     beschreibung))
                (when-let [tasks (story/parse-tasks story)]
                  ($ :div
                     (for [task tasks]
                       ($ Task
                          {:key task
                           :task task}))))
                (when-let [voraussetzungen (-> story :voraussetzungen)]
                  ($ :div
                     ($ :span {:className "b"
                               :style {:white-space "pre-wrap"}}
                        "Voraussetzungen: ")
                     (-> voraussetzungen)))
                ($ :div
                   {:style {:display "grid"
                            :grid-gap "8px"
                            :grid-template-columns "1fr 1fr"
                            :text-align "right"
                            :color "grey"}}
                   ($ :div
                      (when-let [aufwand (-> story :aufwandschaetzung)]
                        ($ :span
                           aufwand " Std geschätzt")))
                   ($ :div
                      (when-let [aufwand (-> story :aufwand)]
                        ($ :span aufwand " Std geleistet"))))
                (when-let [klaerungsbedarf (-> story :klaerungsbedarf)]
                  ($ :div
                     {:style {:color (-> ^js theme .-palette .-primary .-main)
                              :white-space "pre-wrap"}}
                     ($ :span {:className "b"}
                        "Klärungsbedarf: ")
                     (-> klaerungsbedarf))))
             #_(ui/data story))))))

(def-ui StoryCards [storys]
  ($ ui/Stack
     (for [story (->> storys (sort-by story/num))]
       ($ StoryCard
          {:key (-> story :id)
           :story story}))))

(defn sprint-card [sprint projekt storys uid expand]
  (let [sprint-id (-> sprint :id)

        stunden-geschaetzt (reduce (fn [aufwand story]
                                     (when aufwand
                                       (if (= sprint-id (-> story :sprint-id))
                                         (let [story-aufwand (-> story :aufwandschaetzung)]
                                           (+ aufwand story-aufwand))
                                         aufwand)))
                                   0 storys)

        stunden-geleistet (reduce (fn [aufwand story]
                                    (when aufwand
                                      (if (= sprint-id (-> story :sprint-id))
                                        (let [story-aufwand (-> story :aufwand)]
                                          (+ aufwand story-aufwand))
                                        aufwand)))
                                  0 storys)]
    ($ mui/Card
       {:className "StoryMap-SprintCard"}
       ;; (ui/data sprint)
       (ui/div
        {:padding "8px 16px"}
        (if (> sprint-id 9999)
          (ui/div
           {:padding "8px 0"}
           (str "Sprint #" sprint-id " - Noch nicht eingeplant"))
          (ui/grid
           "repeat(9, minmax(min-content, max-content))"
           {:align-items "center"
            :grid-gap "32px"}
           (when expand
             ($ ui/IconButton
                {:icon "apps"
                 :size "small"
                 :onClick #(expand sprint-id)}))
           (ui/div "Sprint #" sprint-id)
           (when-let [entwickler (-> sprint :entwickler)]
             (ui/div entwickler))
           (when (> stunden-geschaetzt 0)
             (ui/div
              {:font-weight "normal"
               :color "#eee"}
              (ui/span {:font-weight 900} "Schätzung: ")
              stunden-geschaetzt " Stunden / " (/ stunden-geschaetzt 8) " Tage"))
           (when (> stunden-geleistet 0)
             (ui/div
              {:font-weight "normal"
               :color "#eee"}
              (ui/span {:font-weight 900} "Leistung: ")
              stunden-geleistet " Stunden / " (/ stunden-geleistet 8) " Tage"))
           (when-let [datum (-> sprint sprint/datum-abgeschlossen)]
             (ui/div
              {:font-weight "normal"
               :color "#eee"}
              "Abgeschlossen am " datum))
           (when (projekt/developer-uid? projekt uid)
             ($ ui/CommandButton
                {:as-icon? true
                 :color "default"
                 :size "small"
                 :command commands/update-sprint
                 :context {:projekt projekt
                           :sprint sprint}}))))))))

(defn features-row [projekt uid {:keys [feature-ids]} sprint-id]
  ($ :tr
     (for [feature-id feature-ids]
       ($ :th {:key feature-id}
          ($ mui/Card
             {:className "StoryMap-FeatureCard"}
             ($ :div
                {:style {:display "flex"
                         :justify-content "space-between"
                         :align-items "center"
                         :padding "0 1rem"}}
                ($ :div feature-id)
                (when (projekt/developer-uid? projekt uid)
                  ($ ui/CommandButton
                     {:command commands/add-story
                      :context {:form-defaults
                                {:sprint-id sprint-id
                                 :feature-id feature-id}}
                      :as-icon? true
                      :size "small"}))))))))

(def-ui Storymap [projekt uid]
  {:from-context [projekt uid]}
  (let [storys (-> projekt :storys vals)
        storymap (core/storymap projekt storys)
        feature-ids (->> storymap :feature-ids)
        sprints-ids (-> storymap :sprints-ids)
        [expanded-sprint-ids set-expended-sprint-ids] (ui/use-state #{})
        expand #(set-expended-sprint-ids (conj expanded-sprint-ids %))]
    (ui/stack
     ;; (ui/data (-> projekt :sprints))
     ;; (ui/data (macroexpand-1 '(def-ui Hello []
     ;;                            {:wrap-memo-props [story]}
     ;;                            nil)))
     ;; (ui/data uid)
     ;; (ui/data (-> projekt :developers))
     ;; (ui/data {:sprints-ids sprints-ids})
     ($ :table

        ($ :thead)

        ($ :tbody
           (for [sprint-id sprints-ids]
             (let [sprint (or (-> projekt :sprints (get sprint-id))
                              {:id sprint-id
                               :db/ref [(-> projekt :db/ref) :sprints sprint-id]})
                   expanded? (or (contains? expanded-sprint-ids sprint-id)
                                 (not (-> sprint sprint/closed?)))]
               (<>
                {:key sprint-id}
                ($ :tr
                   ($ :td
                      {:colSpan (count feature-ids)}
                      (sprint-card sprint projekt storys uid (when-not expanded? expand))))
                (when expanded?
                  (features-row projekt uid storymap sprint-id))
                (when expanded?
                  ($ :tr
                     (for [feature-id feature-ids]
                       ($ :td {:key feature-id
                               :style {:vertical-align "top"
                                       ;; :padding "0.5rem"
                                       }}
                          ($ StoryCards
                             {:storys (get-in storymap
                                              [:storys-by-sprint-and-feature
                                               [sprint-id feature-id]])}))))))))))
     ($ :div
        (when (projekt/developer-uid? projekt uid)
          ($ ui/CommandButton
             {:command commands/add-story})))
     ($ :div
        {:style {:display "none"}}
        ($ :textarea
           {:default-value (u/->edn projekt)})))))

(def-page storymap-page
  {:path "/ui/projekt/storymap"
   :max-width false
   :content Storymap
   :wait-for [:projekt]
   :update-context (fn [context]
                     (assoc context :projekt (ui/use-singleton-doc projekt/Projekt)))})

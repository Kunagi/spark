(ns projekt.storymap-page
  (:require

   ["@material-ui/core" :as mui]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.db :as db]
   [spark.ui :as ui :refer [def-page def-ui $ <>]]

   [projekt.core :as core]
   [projekt.story :as story]
   [projekt.sprint :as sprint]
   [projekt.projekt :as projekt]))

(defn show-add-story-form> [projekt]
  (ui/show-form-dialog>
   {:fields [story/Bez story/Beschreibung
             story/Tasks
             story/Voraussetzungen
             story/Feature-id story/Sprint-id
             story/Aufwandschaetzung]
    :submit (fn [values]
              (let [story (-> values
                              (assoc :id (projekt/next-story-id projekt)))]
                (db/add-child> projekt [:storys] story)))}))

(def-ui StoryDeleteButton [story]
  (let [hide-dialog (ui/use-hide-form-dialog)]
    ($ ui/Button
       {:text "Löschen"
        :icon :delete
        :color :default
        :on-click (fn []
                    (db/update> story {:deleted true})
                    (hide-dialog))})))

(defn show-update-story-form> [projekt story uid]
  (let [fields (if (projekt/developer-uid? projekt uid)
                 [story/Bez story/Beschreibung
                  story/Tasks
                  story/Voraussetzungen
                  story/Klaerungsbedarf
                  story/Feature-id story/Sprint-id
                  story/Aufwandschaetzung
                  story/Aufwand]
                 [story/Klaerungsbedarf])]
    (ui/show-form-dialog>
     {:fields fields
      :values story
      :submit #(db/update> story %)
      :extra-buttons ($ StoryDeleteButton {:story story})})))

(defn show-update-sprint> [sprint]
  (let [fields [sprint/Entwickler
                sprint/Datum-abgeschlossen]]
    (ui/show-form-dialog>
     {:fields fields
      :values sprint
      :submit (fn [values]
                (log ::update-sprint
                     :sprint sprint
                     :values values)
                (db/update> sprint values))})))

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

(def-ui StoryCard [story projekt uid]
  {:from-context [uid]
   :wrap-memo-props [story]}
  (log ::StoryCard--render
       :story (-> story story/num)
       :projekt (-> projekt :id))
  (let [theme (ui/use-theme)]
    ($ mui/Card
       ($ mui/CardActionArea
          {:onClick #(show-update-story-form> projekt story uid)}
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

(def-ui StoryCards [storys projekt]
  (ui/stack
   (for [story (->> storys (sort-by story/num))]
     ($ StoryCard
        {:key (-> story :id)
         :story story
         :projekt projekt}))))

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
             ($ ui/Button
                {:icon :edit
                 :color "default"
                 :size "small"
                 :on-click #(show-update-sprint> sprint)}))))))))

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
                  ($ ui/Button
                     {:on-click #(show-add-story-form> projekt)
                      :icon :add
                      :size "small"}))))))))

(def-ui Storymap [projekt uid]
  {:from-context [projekt uid]}
  (let [storys (-> projekt projekt/storys)
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
                                               [sprint-id feature-id]])
                              :projekt projekt}))))))))))
     ($ :div
        (when (projekt/developer-uid? projekt uid)
          ($ ui/Button
             {:text "Neue Story"
              :icon :add
              :on-click #(show-add-story-form> projekt)})))
     ($ :div
        {:style {:display "none"}}
        ($ :textarea
           {:default-value (u/->edn projekt)})))))

(def-page storymap-page
  {:path "/ui/projekt/storymap"
   :max-width false
   :content Storymap
   :wait-for [:projekt]
   :title "Project StoryMap"
   :update-context (fn [context]
                     (assoc context :projekt (ui/use-singleton-doc projekt/Projekt)))})

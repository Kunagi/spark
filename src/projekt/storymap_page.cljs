(ns projekt.storymap-page
  (:require

   ["@material-ui/core" :as mui]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.local :as local]
   [spark.db :as db]
   [spark.ui :as ui :refer [def-page def-ui $ <>]]

   [projekt.core :as core]
   [projekt.story :as story]
   [projekt.sprint :as sprint]
   [projekt.projekt :as projekt]
   [clojure.string :as str]))

(defn show-add-story-form> [projekt values]
  (ui/show-form-dialog>
   {:fields [story/Bez story/Beschreibung
             story/Tasks
             story/Voraussetzungen
             story/Feature-id story/Sprint-id
             story/Aufwandschaetzung]
    :values values
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
                  story/Aufwand
                  story/Prio]
                 [story/Prio
                  story/Klaerungsbedarf])]
    (ui/show-form-dialog>
     {:fields fields
      :values story
      :submit #(db/update> story %)
      :extra-buttons (when (projekt/developer-uid? projekt uid)
                       ($ StoryDeleteButton {:story story}))})))

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

(defn format-klaerungsbedarf [s]
  (ui/div
   {:white-space "pre-wrap"}
   (ui/div
    {:font-size "80%"
     :font-weight 900}
    "Klärungsbedarf")
   (for [[idx line]  (map-indexed vector (str/split-lines s))]
     (let [response? (str/starts-with? line "> ")]
       (ui/div
        {:id idx
         :color (if response? "green" "red")}
        line
        (when response?
          (ui/div {:height 8})))))))

(def-ui StoryCard [story projekt uid]
  {:from-context [uid]
   :wrap-memo-props [story]}
  (log ::StoryCard--render
       :story (-> story story/num)
       :projekt (-> projekt :id))
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
                          :grid-template-columns "auto auto auto"}}
                 (ui/div
                  {:color "grey"}
                  (when-let [prio (-> story story/prio)]
                    (ui/div "Prio " prio)))
                 (ui/div
                  {:text-align :right
                   :color "grey"}
                  (when-let [aufwand (-> story :aufwandschaetzung)]
                    ($ :span
                       aufwand " Std geschätzt")))
                 (ui/div
                  {:text-align :right
                   :color "grey"}
                  (when-let [aufwand (-> story :aufwand)]
                    ($ :span aufwand " Std geleistet"))))
              (when-let [s (-> story :klaerungsbedarf)]
                (format-klaerungsbedarf s)))
           #_(ui/data story)))))

(def-ui StoryCards [storys projekt]
  (ui/stack
   (for [story (->> storys (sort-by story/sort-value))]
     ($ StoryCard
        {:key (-> story :id)
         :story story
         :projekt projekt}))))

(defn sprint-card [sprint projekt uid]
  (let [sprint-id (-> sprint :id)
        storys (-> projekt projekt/storys)
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

(defn features-row [projekt uid feature-ids sprint-id]
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
                     {:on-click #(show-add-story-form>
                                  projekt
                                  {:feature-id feature-id
                                   :sprint-id sprint-id})
                      :icon :add
                      :size "small"}))))))))

(defonce SELECTED_SPRINT_ID (atom nil))

(def use-selected-sprint-id (ui/atom-hook SELECTED_SPRINT_ID))

(defn use-current-sprint-id [storymap]
  (let [selected-sprint-id (use-selected-sprint-id)]
    (when-not (= "_alle" selected-sprint-id)
      (or selected-sprint-id
          (->> storymap
               :sprints-ids
               (map (fn [sprint-id]
                      (-> storymap :sprints (get sprint-id))))
               (remove sprint/datum-abgeschlossen)
               first
               :id)
          (-> storymap :sprints-ids first)))))

(def-ui SprintSelector [storymap]
  (let [current-sprint-id (use-current-sprint-id storymap)
        {:keys [sprints-ids]} storymap]
    (ui/stack
     ($ mui/Select
        {:variant "outlined"
         :size "small"
         :value (or current-sprint-id "_alle")
         :onChange #(->> % .-target .-value (reset! SELECTED_SPRINT_ID))}
        (for [sprint-id sprints-ids]
          (let [sprint (-> storymap :sprints (get sprint-id))]
            ($ mui/MenuItem
               {:key sprint-id
                :value sprint-id}
               (str "Sprint #" sprint-id
                    (when-let [s (-> sprint sprint/entwickler)]
                      (str " | " s))
                    (when-let [s (-> sprint sprint/datum-abgeschlossen)]
                      (str " | " (-> s local/format-date)))))))
        ($ mui/MenuItem
           {:value "_alle"}
           "* Alle Sprints *"))
     ;; (ui/DEBUG (-> storymap :sprints))
     ;; (ui/DEBUG sprints-ids)
     )))

(def-ui SprintTableRows [sprint storymap projekt standalone uid]
  {:from-context [uid]}
  (let [sprint-id (-> sprint :id)
        feature-ids (-> storymap :feature-ids)
        feature-ids (if-not standalone
                      feature-ids
                      (->> feature-ids
                           (remove (fn [feature-id]
                                     (empty?
                                      (get-in storymap
                                              [:storys-by-sprint-and-feature
                                               [sprint-id feature-id]]))))))]
    (<>
     {:key sprint-id}
     ($ :tr
        ($ :td
           {:colSpan (count feature-ids)}
           (sprint-card sprint projekt uid)))
     (features-row projekt uid feature-ids sprint-id)
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
                 :projekt projekt})))))))

(def-ui Storymap [projekt uid]
  {:from-context [projekt uid]}
  (let [storys (-> projekt projekt/storys)
        storymap (core/storymap projekt storys)
        sprints-ids (-> storymap :sprints-ids)
        current-sprint-id (use-current-sprint-id storymap)]
    (ui/stack
     ;; (ui/data (-> projekt :sprints))
     ;; (ui/data (macroexpand-1 '(def-ui Hello []
     ;;                            {:wrap-memo-props [story]}
     ;;                            nil)))
     ;; (ui/data uid)
     ;; (ui/data (-> projekt :developers))
     ;; (ui/data {:sprints-ids sprints-ids})
     ($ SprintSelector {:storymap storymap})
     ($ :table

        ($ :thead)

        ($ :tbody
           (if-not current-sprint-id
             (for [sprint-id sprints-ids]
               (let [sprint (-> storymap :sprints (get sprint-id))]
                 ($ SprintTableRows {:sprint sprint
                                     :storymap storymap
                                     :projekt projekt
                                     :standalone false})))
             ($ SprintTableRows {:sprint (-> storymap :sprints (get current-sprint-id))
                                 :storymap storymap
                                 :projekt projekt
                                 :standalone true}))))

     ($ :div
        (when (projekt/developer-uid? projekt uid)
          ($ ui/Button
             {:text "Neue Story"
              :icon :add
              :on-click #(show-add-story-form>
                          projekt
                          {:sprint-id current-sprint-id})})))
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

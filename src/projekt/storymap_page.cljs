(ns projekt.storymap-page
  (:require

   ["@mui/material/colors" :as colors]
   ["@mui/material" :as mui]
   [tick.core :as tick]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.local :as local]
   [spark.db :as db]
   [spark.ui :as ui :refer [def-page def-ui $ <>]]

   [projekt.core :as core]
   [projekt.story :as story]
   [projekt.sprint :as sprint]
   [projekt.projekt :as projekt]
   [clojure.string :as str]
   [kunagi.mui.api :as kui]))

(defn show-add-story-form> [projekt values]
  (ui/show-form-dialog>
   {:fields [story/Bez story/Beschreibung
             story/Tasks
             story/Voraussetzungen
             story/Feature-id story/Sprint-id
             story/Aufwandschaetzung
             story/Prio]
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
                 [story/Bez
                  story/Beschreibung
                  story/Voraussetzungen
                  story/Feature-id story/Sprint-id
                  story/Aufwandschaetzung
                  story/Aufwand
                  story/Prio
                  story/Klaerungsbedarf
                  story/Hindernis
                  story/Tasks]
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
                sprint/Datum-Beginn
                sprint/Datum-Ende
                sprint/Tagesleistung
                sprint/Datum-Abgeschlossen]]
    (ui/show-form-dialog>
     {:fields fields
      :values sprint
      :submit (fn [values]
                (log ::update-sprint
                     :sprint sprint
                     :values values)
                (db/update> sprint values))})))

(defn >task [task]
  ($ :div
     {:key task
      :style {:display :flex}}
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
    {:font-weight 900
     :color (-> colors .-orange (aget 900))}
    "Klärungsbedarf")
   (for [[idx line]  (map-indexed vector (str/split-lines s))]
     (let [response? (str/starts-with? line "> ")]
       (ui/div
        {:id idx
         :color (when response?
                  (-> colors .-blue (aget 600)))}
        line
        (when response?
          (ui/div {:height 8})))))))

(defn format-hindernis [s]
  (ui/div
   {:white-space :pre-wrap}
   (ui/div
    {:font-weight 900
     :color (-> colors .-red (aget 900))}
    "Hindernisse")
   (ui/div
    s)))

(defonce EXPANDED_STORYS (atom #{}))

(def-ui StoryCard [story projekt sprint lowest-prio uid arbeitstage]
  {:from-context [uid]
   :wrap-memo-props [story lowest-prio arbeitstage]}
  (let [expanded-storys (ui/use-atom EXPANDED_STORYS)
        expanded? (contains? expanded-storys (-> story :id))

        hindernis? (-> story story/hindernis boolean)
        ungeschaetzt? (-> story story/restaufwand nil?)
        completed? (-> story story/completed?)
        prio (-> story story/prio)
        next? (and (not (-> sprint sprint/datum-abgeschlossen))
                   (-> sprint sprint/datum-beginn)
                   (= prio lowest-prio)
                   (not completed?))
        aufwand (max (-> story story/restaufwand)
                     (-> story story/aufwand)
                     1)
        dev? (projekt/developer-uid? projekt uid)
        collapsed? (and completed? dev? (not expanded?))]
    (log ::StoryCard--render
         :story (-> story story/num)
         :projekt (-> projekt :id))
    ($ ui/Card
       {:sx {:background-color (cond
                                 completed? (-> colors .-green (aget 50))
                                 (nil? prio) (-> colors .-grey (aget 200)))}}
       ;; (ui/DEBUG arbeitstage)
       ($ mui/CardActionArea
          {:onClick (fn []
                      (if collapsed?
                        (swap! EXPANDED_STORYS conj (-> story :id))
                        (show-update-story-form> projekt story uid)))}
          ($ mui/CardContent
             (ui/stack

              ;; Prio | # | Aufwände
              (ui/grid-3
               [:max-content :auto :max-content :max-content :max-content]
               {:align-items :center}

               (ui/div
                "#" (-> story :id))
               (ui/div)
               (ui/div
                {:color (if next? (-> colors .-blue (aget 600)) "grey")
                 :font-weight (when next? 900)}
                (when prio
                  (str "Prio " prio)))
               (ui/div
                {:color "grey"}
                (when-let [tage (-> story :fertig-in-tagen)]
                  (if-let [date (nth arbeitstage tage)]
                    (let [ende (-> sprint sprint/datum-ende u/->date)
                          nach-ende? (when (and ende (-> date (tick/> ende)))
                                       (-> date (tick/> ende)))]
                      (if nach-ende?
                        (ui/span
                         {:color (-> colors .-red (aget 900))
                          :font-weight :bold}
                         (-> date local/format-date))
                        (-> date local/format-date)))

                    (str "in " tage " AT"))))
               (ui/div
                {:color "grey"}
                (ui/flex
                 {:align-items :center}
                 (ui/div
                  (or (-> story :aufwand) "0")
                  " / "
                  (if-let [x (-> story :aufwandschaetzung)]
                    x
                    (ui/span
                     {:color (-> colors .-red (aget 900))
                      :font-weight 900}
                     "???"))
                  " Std")
                 ;; (ui/div
                 ;;  {:background-color "#aaa"
                 ;;   :width            (+ aufwand 2)
                 ;;   :height           (+ aufwand 2)})
                 )))

              ($ :div
                 {:style {:text-align "center"
                          :min-width "200px"
                          :font-weight "bold"}}
                 (-> story :bez))
              (when-let [beschreibung (-> story :beschreibung)]
                ($ :div
                   {:style {:text-align "center"
                            :white-space "pre-wrap"}}
                   beschreibung))

              (when-not collapsed?
                (ui/stack
                 (when-let [tasks (story/parse-tasks story)]
                   ($ :div
                      (for [task tasks]
                        (>task task))))
                 (when-let [voraussetzungen (-> story :voraussetzungen)]
                   ($ :div
                      ($ :span {:className "b"
                                :style {:white-space "pre-wrap"}}
                         "Voraussetzungen: ")
                      (-> voraussetzungen)))
                 (when-let [s (-> story story/hindernis)]
                   (format-hindernis s))))

              (when-let [s (-> story :klaerungsbedarf)]
                (format-klaerungsbedarf s)))

             ;; (ui/DEBUG {:restaufwand (-> story story/restaufwand)
             ;;            :tag (-> story :fertig-in-tagen)})
             ;;
             #_(ui/data story))))))

(def-ui StoryCards [storys projekt sprint lowest-prio arbeitstage]
  (ui/stack
   (for [story (->> storys (sort-by story/sort-value))]
     (ui/div
      {:key (-> story :id)}
      ($ kui/ErrorBoundary
         ($ StoryCard
            {:key (-> story :id)
             :story story
             :projekt projekt
             :sprint sprint
             :lowest-prio lowest-prio
             :arbeitstage arbeitstage}))))))

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
    ($ ui/Card
       {:class "StoryMap-SprintCard"
        :on-click #(when (projekt/developer-uid? projekt uid)
                     (show-update-sprint> sprint))}
       ;; (ui/data sprint)
       (ui/div
        {:padding "8px 16px"}
        (if (> sprint-id 9999)
          (ui/div
           {:padding "8px 0"}
           (str "Sprint #" sprint-id " - Noch nicht eingeplant"))
          (ui/flex
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
              "Abgeschlossen am " datum))))))))

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

(defonce SEARCH_OPTS (atom {:auch-abgeschlossene false}))
(def use-search-opts (ui/atom-hook SEARCH_OPTS))
(defn use-search-text []
  (-> (use-search-opts) :text))

(defonce SELECTED_SPRINT_ID (atom nil))
(def use-selected-sprint-id (ui/atom-hook SELECTED_SPRINT_ID))

(defn use-current-sprint-id [storymap]
  (let [selected-sprint-id (use-selected-sprint-id)
        search-text (use-search-text)]
    (when (and (not= "_alle" selected-sprint-id)
               (str/blank? search-text))
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
    (ui/div
     {:max-width "300px"}
     (ui/stack
      ($ mui/FormControl
         {:size "small"}
         ($ mui/Select
            {:variant "outlined"
             :size "small"
             :fullWidth false
             :value (or current-sprint-id "_alle")
             :onChange (fn [^js event]
                         (reset! SEARCH_OPTS nil)
                         (->> event .-target .-value (reset! SELECTED_SPRINT_ID)))}
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
               "* Alle Sprints *")))
      ;; (ui/DEBUG (-> storymap :sprints))
      ;; (ui/DEBUG sprints-ids)
      ))))

(defn show-search-form> []
  (ui/show-form-dialog
   {:fields [{:id :text
              :label "Suchtext oder Nummer"}
             {:id :auch-abgeschlossene
              :label "Auch abgeschlossene Sprints durchsuchen"
              :type :checkbox}]
    :values @SEARCH_OPTS
    :submit #(reset! SEARCH_OPTS %)}))

(def-ui Controls [storymap]
  (let [search-text (use-search-text)]
    (ui/div
     {:max-width "90vw"}
     (ui/flex
      (when-not search-text
        ($ SprintSelector {:storymap storymap}))
      ($ ui/Button
         {;; :text "Suche"
          :icon :search
          :color :default
          :on-click #(show-search-form>)})
      (when search-text
        ($ ui/Button
           {:icon :delete_forever
            :text (str "\"" search-text "\"")
            :color :default
            :on-click #(reset! SEARCH_OPTS nil)}))))))

(defn story-projections [storys sprint]
  (when (and (-> sprint sprint/datum-beginn)
             (-> sprint sprint/tagesleistung)
             (not (-> sprint sprint/datum-abgeschlossen)))
    (let [tagesleistung (-> sprint sprint/tagesleistung)
          date-start (tick/max (-> sprint sprint/datum-beginn tick/date)
                               (tick/date))
          storys (->> storys
                      (sort-by story/sort-value)
                      (remove story/completed?)
                      ;; (take-while story/restaufwand)
                      (map-indexed vector)
                      (map (fn [[idx story]]
                             (assoc story :idx idx))))
          arbeitsstunden (->> storys
                              (mapcat (fn [story]
                                        (repeat (-> story story/restaufwand (max 1)) (-> story :id)))))
          arbeitstag-by-sprint (->> arbeitsstunden
                                    (reduce (fn [[tag sprint->tag] sprint-id]
                                              (let [tag (-> tag
                                                            (update :sprint-ids conj sprint-id)
                                                            (update :reststunden dec))
                                                    sprint->tag (assoc sprint->tag
                                                                       sprint-id (-> tag :idx inc))]
                                                (if (-> tag :reststunden pos?)
                                                  [tag sprint->tag]
                                                  [{:idx (-> tag :idx inc)
                                                    :sprint-ids #{}
                                                    :reststunden tagesleistung}
                                                   sprint->tag])))
                                            [{:idx 0
                                              :sprint-ids #{}
                                              :reststunden tagesleistung}
                                             {}])
                                    second)
          ;; tage-offset-by-sprint (->> )
          ]
      {:date-start date-start
       :storys storys
       :arbeitsstunden arbeitsstunden
       :arbeitstag arbeitstag-by-sprint})))

(def-ui SprintTableRows [sprint storymap projekt standalone uid]
  {:from-context [uid]}
  (let [instant (tick/instant)
        sprint-id (-> sprint :id)
        feature-ids (-> storymap :feature-ids)
        feature-ids (if-not standalone
                      feature-ids
                      (->> feature-ids
                           (remove (fn [feature-id]
                                     (empty?
                                      (get-in storymap
                                              [:storys-by-sprint-and-feature
                                               [sprint-id feature-id]]))))))
        storys-in-sprint (->> storymap
                              :storys
                              (filter (fn [story]
                                        (-> story story/sprint-id (= sprint-id)))))
        lowest-prio (->> storys-in-sprint
                         (remove story/completed?)
                         (map story/prio)
                         (reduce (fn [acc prio]
                                   (cond
                                     (and acc prio) (min acc prio)
                                     acc acc
                                     prio prio))
                                 nil))
        arbeitstage (sprint/arbeitstage-ab sprint instant)
        story-projections (story-projections storys-in-sprint sprint)]
    (<>
     {:key sprint-id}
     ($ :tr
        ($ :td
           {:colSpan (count feature-ids)}
           (sprint-card sprint projekt uid)
           ;; (ui/DEBUG arbeitstage)
           ;; (ui/DEBUG story-projections)
           ))
     (features-row projekt uid feature-ids sprint-id)
     ($ :tr
        (for [feature-id feature-ids]
          ($ :td {:key feature-id
                  :style {:vertical-align "top"
                           ;; :padding "0.5rem"
                          }}
             ($ StoryCards
                {:storys (->> (get-in storymap
                                      [:storys-by-sprint-and-feature
                                       [sprint-id feature-id]])
                              (map (fn [story]
                                     (assoc story
                                            :fertig-in-tagen (-> story-projections :arbeitstag (get (-> story :id)))))))
                 :projekt projekt
                 :sprint sprint
                 :lowest-prio lowest-prio
                 :arbeitstage arbeitstage})))))))

(defn filter-storys [projekt storys search-opts]
  (let [search-text (-> search-opts
                        :text
                        str/trim
                        str/lower-case)
        ;; words (->> (str/split search-text #"\s")
        ;;            (remove nil?)
        ;;            (map str/lower-case))
        storys (if (-> search-opts :auch-abgeschlossene)
                 storys
                 (->> storys
                      (remove (fn [story]
                                (let [sprint-id (-> story story/sprint-id)
                                      sprint (-> projekt
                                                 :sprints
                                                 (get sprint-id))
                                      abgeschlossen? (-> sprint sprint/datum-abgeschlossen)]
                                  abgeschlossen?)))))]
    (->> storys
         (filter #(story/matches-suchtext % search-text)))))

(def-ui Storymap [projekt uid]
  {:from-context [projekt uid]}
  (let [search-opts (use-search-opts)
        search-text (-> search-opts :text)
        storys (-> projekt projekt/storys)
        storys (if (str/blank? search-text)
                 storys
                 (filter-storys projekt storys search-opts))
        storymap (core/storymap projekt storys)
        sprints-ids (-> storymap :sprints-ids)
        current-sprint-id (use-current-sprint-id storymap)]
    (ui/stack
     ;; (ui/DEBUG storys)
     ;; (ui/data (-> projekt :sprints))
     ;; (ui/data (macroexpand-1 '(def-ui Hello []
     ;;                            {:wrap-memo-props [story]}
     ;;                            nil)))
     ;; (ui/data uid)
     ;; (ui/data (-> projekt :developers))
     ;; (ui/data {:sprints-ids sprints-ids})
     ($ Controls {:storymap storymap})
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

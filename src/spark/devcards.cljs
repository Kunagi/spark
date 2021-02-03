(ns spark.devcards
  (:require
   [spark.ui :as ui :refer [defnc $]])
  )


(defnc Example [{:keys [devcard example label]}]
  (ui/stack
     (ui/div "Example " label)
     (ui/div
      {:display :grid
       :grid-template-columns "1fr 1fr"
       :grid-gap "8px"}
      (ui/div
       {:white-space "pre-wrap"
        :font-family "monospace"
        :overflow "auto"
        :background-color "#333"
        :color "#eee"
        :padding "8px"
        :border-radius "4px"}
       (-> example :code))
      (ui/div
       {:display "flex"
        :place-content "center"
        :place-items "center"}
       (ui/div
        {:border "1px dotted #ddd"
         :padding "8px"
         :max-width "600px"
         :margin "0 auto"
         :overflow "auto"}
        ((-> example :f)))))))


(defnc Devcard [{:keys [devcard]}]
  (ui/stack-4
   (for [[idx example] (map-indexed vector (-> devcard :examples))]
     ($ Example {:key idx
                 :devcard devcard
                 :example example
                 :label (str "#" (inc idx))}))))


(defonce SELECTED_DEVCARD_ID (atom nil))

(def use-selected-devcard-id (ui/atom-hook SELECTED_DEVCARD_ID))

(defnc Selector []
  (let [devcards (->> ui/DEVCARDS deref vals (group-by :namespace))
        groups (-> devcards keys sort)
        selected-devcard-id (use-selected-devcard-id)]
    (ui/div
     (ui/div
      {:position "fixed"}
      ($ ui/SimpleCard
         (ui/div
          {:width "130px"
           :max-height "80vh"
           :overflow "auto"}
          (for [group groups]
            (ui/div
             {:key group}
             (ui/div {:font-weight 900} (-> group))
             (for [devcard (->> (get devcards group)
                                (sort-by :symbol))]
               (let [id (-> devcard :id)
                     selected? (= id selected-devcard-id)]
                 ($ :a
                    {:key id
                     :onClick #(reset! SELECTED_DEVCARD_ID id)
                     :style {:cursor "pointer"}}
                    (ui/div
                     {:padding-left "4px"
                      :color (when selected? "black")}
                     (-> devcard :symbol)))))))))))))


(defnc DevcardsPageContent []
  (let [selected-devcard-id (use-selected-devcard-id)
        selected-devcard (-> ui/DEVCARDS deref (get selected-devcard-id))]
    (ui/grid ["170px" :auto] {:grid-gap 8}

             ($ Selector)

             (ui/div {:min-height "100vh"}
                     (when selected-devcard
                       ($ Devcard
                          {:devcard selected-devcard}))))))


(def devcards-page
  {:path "/ui/devcards"
   :max-width false
   :content DevcardsPageContent})


(reset! ui/ADDITIONAL_PAGES [devcards-page])

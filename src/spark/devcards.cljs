(ns spark.devcards
  (:require
   [spark.core :as spark]
   [spark.ui :as ui :refer [defnc $]])
  )

(defn expect [expected provided]
  (if (= expected provided)
    provided
    (throw (ex-info "result does'n meet expectations"
                    {:expected expected
                     :provided provided}))))

(defn- colored-data [color data])

(defnc FnResult [{:keys [example]}]
  (ui/div
   {:border "1px dotted #ddd"
    :padding "8px"
    :max-width "600px"
    :margin "0 auto"
    :overflow "auto"}
   (try
     (let [result ((-> example :f) expect)]
       (ui/colored-data-block nil "#333" "#6f6" result))
     (catch :default error
       (let [ex-data (ex-data error)]
         (if (-> ex-data :expected)
           (ui/stack
            "result:"
            (ui/colored-data-block nil "#c00" "#fff" (-> ex-data :provided))
            "doesn't meet expectations:"
            (ui/colored-data-block nil "#333" "#6f6" (-> ex-data :expected)))
           ($ ui/ErrorInfo {:error error})))))))


(defnc UiResult [{:keys [example]}]
  (ui/div
   {:border "1px dotted #ddd"
    :padding "8px"
    :max-width "600px"
    :margin "0 auto"
    :overflow "auto"}
   ((-> example :f))))


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
       (case (-> devcard :type)
         :ui ($ UiResult {:example example})
         :fn ($ FnResult {:example example})
         (ui/stack
          "Unsupported Devcard Type"
          (ui/data devcard)))))))


(defnc Devcard [{:keys [devcard]}]
  (ui/stack-4
   (for [[idx example] (map-indexed vector (-> devcard :examples))]
     ($ Example {:key idx
                 :devcard devcard
                 :example example
                 :label (str "#" (inc idx))}))))


(defonce SELECTED_DEVCARD_ID (atom nil))

(def use-selected-devcard-id (ui/atom-hook SELECTED_DEVCARD_ID))

(def use-devcards (ui/atom-hook spark/TESTS))

(defnc Selector []
  (let [devcards (->> (use-devcards) vals (group-by :namespace))
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
        selected-devcard (-> (use-devcards) (get selected-devcard-id))]
    (ui/grid ["170px" :auto] {:grid-gap 8}

             ($ Selector)

             (ui/div {:min-height "100vh"}
                     (when selected-devcard
                       ($ Devcard
                          {:devcard selected-devcard}))))))


(def devcards-page
  {:path "/ui/devcards"
   :max-width false
   :content DevcardsPageContent
   :update-context (fn [context]
                     (let [params (ui/use-params)]
                       (js/console.log "UPDATE CONTEXT")
                       (assoc context :developer "witek")))})


(reset! ui/ADDITIONAL_PAGES [devcards-page])

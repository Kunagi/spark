(ns spark.form-ui
  (:require
   [clojure.spec.alpha :as s]

   ["@mui/material" :as mui]

   [kunagi.mui.core :as kui.core :refer [defnc]]
   [kunagi.mui.api :as kui :refer [$ <>]]

   [spark.utils :as u]
   [spark.core :as spark]
   [spark.logging :refer [log]]
   ;; [spark.mui :as ui]
   [spark.form :as form]
   [spark.db :as db]
   [clojure.string :as str]))

(defn DEBUG [v]
  (when goog.DEBUG
    ($ :div
       {:style {:background-color "black"
                :color "#9f9"
                :font-family "monospace"
                :padding "8px"}}
       (u/->edn v))))

(defonce DIALOG_FORMS (atom {}))

(defn close-form-dialog [form-id]
  (swap! DIALOG_FORMS assoc-in [form-id :open?] false)
  (js/setTimeout #(swap! DIALOG_FORMS dissoc form-id) 1000))

(defn show-form-dialog [form]
  (let [form    (form/initialize form)
        form-id (-> form :id)
        form    (assoc form :open? true)]
    (swap! DIALOG_FORMS assoc form-id form)))

(defn show-form-dialog> [form]
  (u/promise>
   (fn [resolve reject]
     (let [form (assoc form :submit (get form :submit identity))]
       (show-form-dialog (assoc form
                                :then resolve
                                :catch reject))))))

(def use-dialog-forms (kui/atom-hook DIALOG_FORMS))

(defonce HIDE_DIALOG (kui/create-context nil))

(defn use-hide-form-dialog []
  (kui/use-context HIDE_DIALOG))

;; (defnc DialogFormsDebugCard []
;;   ($ mui/Card
;;      ($ mui/CardContent
;;         (ui/data (use-dialog-forms)))))

(defmulti create-input (fn [field]
                         (if-let [type (-> field :type)]
                           (if (keyword? type)
                             (name type)
                             type)
                           "text")))

(defmethod create-input "text" [field]
  (let [step (-> field :step)
        inc-dec (fn [amount]
                  (let [value (-> field :value)
                        new-value (+ value amount)
                        min-value (-> field :min)
                        max-value (-> field :max)]
                    (when (and (or (nil? min-value)
                                   (> new-value value)
                                   (>= new-value min-value))
                               (or (nil? max-value)
                                   (< new-value value)
                                   (<= new-value max-value)))
                      ((:on-change field) new-value))))
        start-adornment (or (-> field :start-adornment)
                            #_(when (-> field :plusminus-adornments)
                                ($ mui/Button
                                   {:onClick #(inc-dec -1)
                                    :variant "contained"
                                    :size "small"
                                    :className "FormPlusMinusAdornmentButton"}
                                   "-")))
        start-adornment (when start-adornment
                          ($ :div
                             {:style {:margin-right "8px"}}
                             start-adornment))
        end-adornment (or (-> field :end-adornment)
                          (when (-> field :plusminus-adornments)
                            ($ :div
                               {:style {:display "grid"
                                        :grid-template-columns "max-content max-content"
                                        :grid-gap "8px"}}
                               ($ mui/Button
                                  {:onClick #(inc-dec (if step
                                                        (- 0 step)
                                                        -1))
                                   :variant "contained"
                                   :size "small"
                                   :className "FormPlusMinusAdornmentButton"}
                                  "-")
                               ($ mui/Button
                                  {:onClick #(inc-dec (or step 1))
                                   :variant "contained"
                                   :size "small"
                                   :className "FormPlusMinusAdornmentButton"}
                                  "+"))))
        end-adornment (when end-adornment
                        ($ :div
                           {:style {:margin-left "8px"}}
                           end-adornment))
        input-props (or (-> field :input-props)
                        {})
        input-props (if (and step
                             (not (-> input-props :step)))
                      (assoc input-props :step step) input-props)
        InputProps  {:startAdornment start-adornment
                     :endAdornment end-adornment}]
    ($ :div
       ;; ($ :pre (-> field :on-change str))
       ;; (when goog.DEBUG
       ;;   ($ :div
       ;;      {:style {:padding          "8px"
       ;;               :background-color "black"
       ;;               :color            "#6F6"
       ;;               :font-family      "monospace"}}
       ;;      (u/->edn (:value field))))
       ($ mui/TextField
          {:id              (-> field :id name)
           :name            (-> field :name)
           :autoComplete    (-> field :auto-complete)
           :value           (or (-> field :value) "")
           :required        (-> field :required?)
           :error           (boolean (-> field :error))
           :helperText      (-> field :error)
           :onChange        #((:on-change field)
                              (-> % .-target .-value))
           :onKeyPress      (when-not (-> field :multiline?)
                              #(when (= "Enter" (-> ^js % .-nativeEvent .-code))
                                 ((:on-submit field))))
           :label           (-> field :label)
           :type            (-> field :input-type)
           :multiline       (get field :multiline?)
           :minRows            (get field :rows (when (get field :multiline?) 5))
           :autoFocus       (-> field :auto-focus?)
           :inputProps      (clj->js input-props)
           :InputProps      (clj->js InputProps)
           :InputLabelProps #js {:shrink true}
           :margin          "dense"
           ;; :variant "filled"
           :variant         "outlined"
           :fullWidth       true}))))

(defmethod create-input "autocomplete" [field]
  (let [;; start-adornment (or (-> field :start-adornment))
        ;; start-adornment (when start-adornment
        ;;                   ($ :div
        ;;                      {:style {:margin-right "8px"}}
        ;;                      start-adornment))
        ;; end-adornment (or (-> field :end-adornment))
        ;; end-adornment (when end-adornment
        ;;                 ($ :div
        ;;                    {:style {:margin-left "8px"}}
        ;;                    end-adornment))
        ;; input-props (or (-> field :input-props)
        ;;                 {})
        ;; InputProps  {:startAdornment start-adornment
        ;;              :endAdornment end-adornment}
        use-options (-> field :use-options)
        options (or (when use-options
                      (use-options))
                    (-> field :options)
                    [])]
    ($ :div
       ;; ($ :pre (-> field :on-change str))
       ;; (when goog.DEBUG
       ;;   ($ :div
       ;;      {:style {:padding          "8px"
       ;;               :background-color "black"
       ;;               :color            "#6F6"
       ;;               :font-family      "monospace"}}
       ;;      (u/->edn (:value field))))

       ($ mui/Autocomplete
          {;; :value           (or (-> field :value) "")
           :inputValue (or (-> field :value) "")
              ;; :label           (-> field :label)
           ;; :inputProps      (clj->js input-props)
           ;; :InputProps      (clj->js InputProps)
           ;; :InputLabelProps #js {:shrink true}

           :freeSolo true
           ;; :includeInputInList true

           :onInputChange #(do
                             (when (and % (-> % .-target))
                               ((:on-change field)
                                (-> % .-target .-value))))

           :onChange (fn [^js event ^js new-value]
                       (when new-value
                         ((:on-change field)
                          (if (string? new-value)
                            new-value
                            (-> new-value .-value)))))

           :onKeyPress      (when-not (-> field :multiline?)
                              #(when (= "Enter" (-> ^js % .-nativeEvent .-code))
                                 ((:on-submit field))))

           :options (->> options clj->js)
           :getOptionLabel   (fn [option]
                               (when option
                                 (if (string? option)
                                   option
                                   (-> option .-label))))

           :renderInput (fn [^js props]

                          ;; (js/console.log "!!! before" props)

                          ;; Workaround fixes damaging of styles of other components
                          (-> props .-InputProps .-endAdornment (set! js/undefined))

                          ;; (-> props .-InputProps .-startAdornment
                          ;;     (set! (ui/icon
                          ;;            {:color "#9a958f"}
                          ;;            "search"))
                          ;;     )

                          ;; Prevent Chrome Autocomplete
                          (-> props .-inputProps .-autoComplete (set! "impp"))

                          (-> props .-InputLabelProps .-shrink (set! true))

                          ;; (js/console.log "!!! after" props)

                          ($ mui/TextField
                             {:id (-> field :id name)
                              :name (-> field :name)
                              :label (-> field :label)
                              :required        (-> field :required?)
                              ;; :placeholder "asldkj aslökdjf aslökjdf aöslkj faslökdfj "
                              :type            (-> field :input-type)
                              :autoFocus       (-> field :auto-focus?)
                              :margin          "dense"
                              :variant         "outlined"
                              :fullWidth       true
                              :error           (boolean (-> field :error))
                              :helperText      (-> field :error)

                              :&           props}))
           ;; :options            (clj->js (conj options "powered-by-google")) ; warum manuell?
           ;; :getOptionSelected  #(= (get %1 "place_id")
           ;;                         (get %2 "place_id"))
           ;; :onChange           (fn [^js event ^js new-value]
           ;;                       (when (and new-value
           ;;                                  (.hasOwnProperty new-value "description")
           ;;                                  (-> new-value .-description))
           ;;                         (update-input-value (.-description new-value))
           ;;                         (when on-place-selected
           ;;                           (on-place-selected
           ;;                            {:place_id    (-> new-value .-place_id)
           ;;                             :description (-> new-value .-description)}))))

           ;; :onInputChange      (fn [^js _event new-input-value]
           ;;                       (update-input-value new-input-value)
           ;;                       (when-not (-> new-input-value str/blank?)
           ;;                         (get-place-predictions-throtteled
           ;;                          new-input-value set-options)))
           ;; :renderOption render-google-option
           }))))

;; MuiFormLabel-root MuiInputLabel-root MuiInputLabel-formControl MuiInputLabel-animated MuiInputLabel-marginDense MuiInputLabel-outlined Mui-required Mui-required
;; MuiFormLabel-root MuiInputLabel-root MuiInputLabel-formControl MuiInputLabel-animated MuiInputLabel-shrink      MuiInputLabel-marginDense MuiInputLabel-outlined MuiFormLabel-filled Mui-required Mui-required

(defmethod create-input "eur" [field]
  (create-input (assoc field
                       :type "text"
                       :input-type "text"
                       :end-adornment "€")))

(defmethod create-input "tel" [field]
  (create-input (assoc field
                       :type "text"
                       :input-type "tel")))

(defmethod create-input "time" [field]
  (create-input (assoc field
                       :type "text"
                       :input-type "time")))

(defmethod create-input "date" [field]
  (let [_ nil
        ;; on-change (fn [new-value]
        ;;             ((-> field :on-change) new-value)
        ;;             (when new-value
        ;;               (when (-> field :form :fields count (= 1))
        ;;                 ((-> field :on-submit)))))
        ]
    (create-input (assoc field
                         :type "text"
                         :input-type "date"
                         ;; :on-change on-change
                         ))))

(defmethod create-input "email" [field]
  (create-input (assoc field
                       :type "text"
                       :input-type "email")))

(defmethod create-input "int" [field]
  (let [input-props (-> field :input-props)
        pattern (or (-> input-props :pattern)
                    "[0-9]*")
        input-props (assoc input-props :pattern pattern)
        min-value (-> field :min)
        max-value (-> field :max)
        input-props (assoc input-props
                           :min min-value
                           :max max-value)]
    (create-input (assoc field
                         :type "text"
                         :input-type "number"
                         :input-props input-props))))

(defmethod create-input "number" [field]
  (create-input (assoc field :type "int")))

(defmethod create-input "select" [field]
  (let [html-id     (str "select_" (-> field :id) "_input")
        input-props (-> field :input-props
                        (assoc :id html-id)
                        (assoc :name (-> field :name)))
        value       (-> field :value)
        required?   (-> field :required?)
        label-id (str "select_" (-> field :id) "_label")
        options (-> field :options)
        get-value-from-options (fn [selected-value]
                                 (->> options
                                      (reduce (fn [ret option]
                                                (or ret
                                                    (when (-> option :value str
                                                              (= selected-value))
                                                      (-> option :value))))
                                              nil)))
        nil-option-exists? (->> field :options
                                (filter #(-> % :value str/blank?))
                                seq)
        value-missing-in-options? (and value
                                       (->> options
                                            (filter #(-> % :value (= value)))
                                            empty?))
        options (if value-missing-in-options?
                  (conj options {:value value
                                 :label (str value " ***")})
                  options)]
    ($ :div
       ;; ($ :pre "!" (-> field :error str) "!")
       ($ mui/FormControl
          {:fullWidth true
           :variant      "outlined"
           :margin       "dense"
           :required     required?
           :error (-> field :error boolean)}
          ($ mui/InputLabel
             {:htmlFor html-id
              :shrink  true
              :id label-id}
             (-> field :label))
          ($ mui/Select
             {:label (-> field :label)
              :native       true
              :id           (-> field :id name)
              :labelId label-id
              :name         (-> field :name)
              :defaultValue value
              :inputProps   (clj->js input-props)
              :onChange     #((:on-change field)
                              (-> % .-target .-value get-value-from-options))
              :autoFocus    (-> field :auto-focus?)}
             (when (and (not nil-option-exists?)
                        (or (nil? value) (not required?)))
               ($ :option {:value nil} ""))
             (for [option options]
               ($ :option
                  {:key   (-> option :value)
                   :value (str (or (-> option :value)
                                   ""))}
                  (or (-> option :label)
                      (-> option :value str)))))
          (when-let [error (-> field :error)]
            ($ mui/FormHelperText error))))))

(defmethod create-input "chips" [field]
  ($ mui/Autocomplete
     {:value            (clj->js (-> field :value))
      :onChange         #((:on-change field) (-> %2 js->clj))
      :multiple         true
      :freeSolo         true
      :disableClearable true
      :id               (-> field :id name)
      :name             (-> field :name)
      :options          (clj->js (or (-> field :options)
                                     []))
      :getOptionLabel   identity
      :renderInput      (fn [props]
                          ($ mui/TextField
                             {:label           (-> field :label)
                              :id              (-> props .-id)
                              :name            (-> props .-name)
                              :disabled        (-> props .-disabled)
                              :fullWidth       true
                              :autoFocus       (-> field :auto-focus?)
                              :size            (-> props .-size)
                              :InputLabelProps (-> props .-InputLabelProps)
                              :InputProps      (-> props .-InputProps)
                              :inputProps      (-> props .-inputProps)}))})
  #_($ ChipInput
       {:id           (-> field :id name)
        :name         (-> field :name)
        :defaultValue (clj->js (-> field :value))
        :onChange     #((:on-change field) (-> % js->clj))
        :dataSource   (clj->js ["hallo" "welt"])
        :label        (-> field :label)
        :autoFocus    (-> field :auto-focus?)
        :margin       "dense"
        :fullWidth    true}))

(defmethod create-input "boolean" [field]
  ($ mui/FormControl
     {:component "fieldset"}
     ($ mui/FormLabel
        {:component "legend"}
        (-> field :label))
     ($ mui/RadioGroup
        {:name (-> field :name)
         :defaultValue (if (-> field :value) "true" "false")
         :onChange #((:on-change field) (= "true" (-> % .-target .-value)))}
        ($ mui/FormControlLabel
           {:value "true"
            :label "Ja"
            :control ($ mui/Radio)})
        ($ mui/FormControlLabel
           {:value "false"
            :label "Nein"
            :control ($ mui/Radio)}))))

(defmethod create-input "checkbox" [field]
  (log ::create-input
       :field field)
  ($ mui/FormControl
     {:component "fieldset"
      :error (-> field :error boolean)}
     ($ mui/FormGroup
        ($ :div {:style {:margin-top "8px"}})
        #_($ mui/FormLabel
             {:component "legend"}
             ($ :div
                {:style {:font-size "16px"}}
                (-> field :label)))
        ($ mui/FormControlLabel
           {:label   (-> field :label)
            :control ($ mui/Checkbox
                        {:name     (name (-> field :id))
                         :checked  (boolean (-> field :value))
                         :onChange #(let [checked? (-> % .-target .-checked)
                                          value (boolean checked?)]
                                      ((-> field :on-change) value))})})
        (when-let [error (-> field :error)]
          ($ mui/FormHelperText error)))))

(defnc CheckboxesInput [{:keys [field]}]
  (let [value (into #{} (-> field :value))
        options-expand-limit (-> field :options-expand-limit)
        options (-> field :options)
        expandable? (and options-expand-limit
                         (> (count options) options-expand-limit))
        [expanded? set-expanded] (kui/use-state (when expandable?
                                                  (reduce (fn [ret option]
                                                            (or ret
                                                                (value (-> option :value))))
                                                          false options)))
        visible-options (if (or expanded?
                                (not expandable?))
                          options
                          (take options-expand-limit options))]
    ($ mui/FormControl
       {:component "fieldset"}
       ($ mui/FormGroup
          ($ :div {:style {:margin-top "8px"}})
          ;; (DEBUG {:count (count options)
          ;;         :expandable expandable?
          ;;         :expanded? expanded?
          ;;         :limit options-expand-limit})
          ($ mui/FormLabel
             {:component "legend"}
             ($ :div
                {:style {:font-size "16px"}}
                (-> field :label)))
          (for [option visible-options]
            ($ mui/FormControlLabel
               {:key     (-> option :value)
                :label   (or (-> option :label)
                             (-> option :value str))
                :control ($ mui/Checkbox
                            {:name     (-> option :value str)
                             :checked  (boolean (value (-> option :value)))
                             :onChange #(let [checked? (-> % .-target .-checked)
                                              value    (into #{} (-> field :value))
                                              value    (if checked?
                                                         (conj value (-> option :value))
                                                         (disj value (-> option :value)))]
                                          ((-> field :on-change) value))})}))
          (when (and expandable?
                     (not expanded?))
            ($ mui/Button
               {:onClick #(set-expanded true)
                :variant "contained"
                :size "small"}
               "Mehr anzeigen")
            #_($ :a
                 {:onClick #(set-expanded true)
                  :style {:cursor "pointer"}}
                 "Mehr anzeigen..."))))))

(defmethod create-input "checkboxes" [field]
  (log ::create-input
       :field field)
  ($ CheckboxesInput {:field field}))

(defmethod create-input "ui" [field]
  ($ :div
     (when-let [Component (-> field :component)]
       ($ Component
          {:field field}))
     (when-let [content (-> field :content)]
       content)))

(defnc FormField [{:keys [field form on-submit update-form]}]
  (let [field-id (-> field :id)
        error    (form/field-error form field-id)
        on-change (fn [value]
                    (update-form form/on-field-value-change field-id value))
        Input    ($ :div
                    (create-input
                     (assoc field
                            :form form
                            :error error
                            :on-submit on-submit
                            :on-change on-change))
                    (when-let [helptext (-> field :helptext)]
                      ($ :div
                         {:style {:color "#666"}}
                         helptext)))]
    ($ :div
       {:key field-id}
       (if-let [action (-> field :action)]
         ($ :div
            {:style {:display "flex"}}
            Input
            ($ :div
               {:style {:margin-left "8px"
                        :padding-top "22px"}}
               ($ mui/Button
                  {:onClick (fn [_]
                              (update-form (-> action :f)))
                   :variant "contained"
                   :color   "secondary"
                   :size    "small"}
                  (-> action :label))))
         Input)
       (when-let [input-hint (-> field :input-hint)]
         ($ :div
            {:style {:color "#999"}}
            input-hint)))))

(def DIALOG-CLASS (atom nil))

(defnc Form [{:keys [form set-form on-close on-cancel]}]
  (let [form         (assoc form :update (fn [f]
                                           (set-form (f form))))
        update-form_ (fn [f & args]
                       (set-form (apply f (into [form] args))))

        set-waiting (fn [waiting?]
                      (update-form_ form/set-waiting waiting?))

        set-submitted (fn []
                        (update-form_ form/set-submitted))

        update-form (fn [f & args]
                      (let [result (apply f (into [form] args))]
                        (if (instance? js/Promise result)
                          (do
                            (-> result
                                (.then (fn [f]
                                         (set-waiting false)
                                         (update-form_ f)))
                                (.catch (fn [error]
                                          (js/console.error
                                           "update-form promise failed:" error)
                                          ;; FIXME display error
                                          (set-waiting false))))
                            (-> form
                                (form/set-waiting true)
                                set-form))
                          (set-form result))))

        close     (fn [result]
                    (when on-close (on-close))
                    (set-submitted)
                    (when-let [then (get form :then)]
                      (then result)))

        cancel (fn []
                 (when on-cancel
                   (on-cancel))
                 (close nil))

        on-submit (fn []
                    (let [form (form/on-submit form)]
                      (update-form (fn [_] form))
                      (when-not (form/contains-errors? form)
                        (let [values (form/values form)]
                          (log ::Form--on-submit
                               :form form
                               :values values)
                          (when-let [submit (get form :submit)]
                            (set-waiting true)
                            (try
                              (let [p (u/as> (submit values))
                                    optimistic? (get form :optimistic-submit true)]
                                (when optimistic? (close p))
                                (-> p
                                    (.then (fn [result]
                                             (close result)))
                                    (.catch (fn [error]
                                              (update-form (fn [_]
                                                             (form/set-error form error)))))))
                              (catch :default ex
                                (update-form (fn [_]
                                               (form/set-error form (u/exception-as-text ex)))))))))))]
    (kui/provider
     {:context HIDE_DIALOG
      :value   close}
     (if (-> form :submitted?)
       (-> form :submitted-content)
       ($ :div
          {:style {:display "grid"
                   :grid-gap "16px"}}

          ;; (when goog.DEBUG
          ;;   (when-let [data (-> form :debug-data)]
          ;;     ($ :div
          ;;        {:style {:padding          "8px"
          ;;                 :background-color "black"
          ;;                 :color            "#6F6"
          ;;                 :font-family      "monospace"}}
          ;;        (u/->edn data))))

          ($ :div
             {:style {:display "grid"
                      :grid-gap "8px"}}
             (for [field (get form :fields)]
               ($ FormField
                  {:key         (-> field :id)
                   :field       field
                   :form        form
                   :on-submit   on-submit
                   :update-form update-form})))

          (get form :content)
          ;; (ui/data form)

          (when-let [error (-> form form/error)]
            ($ :div
               {:style {:padding "16px"
                        :background-color "red"
                        :color "white"
                        :font-weight 900
                        :border-radius "8px"}}
               (str error)))

          ($ :div
             {:style {:display               "grid"
                      :grid-template-columns "max-content auto max-content"
                      :grid-gap              "8px"}}
             ($ :div
                (when-not (-> form form/waiting?)
                  (-> form :extra-buttons)))
             ($ :div)
             ($ :div
                {:style {:display               "grid"
                         :grid-template-columns "max-content max-content"
                         :grid-gap              "8px"}}
                (when-not (-> form :cancel-disabled)
                  ($ mui/Button
                     {:onClick #(cancel)}
                     "Abbrechen"))
                ($ mui/Button
                   {:onClick on-submit
                    :variant "contained"
                    :color   "primary"
                    :disabled (-> form form/waiting?)}
                   (or (-> form :submit-button-text)
                       "Ok"))))

          (when (-> form :waiting?)
            ($ :div
               {:style {:min-height "4px"}}
               ($ mui/LinearProgress))))))))

(defnc FormDialog [{:keys [form]}]
  (let [open? (-> form :open? boolean)
        on-close (fn []
                   (close-form-dialog (-> form :id)))
        [form set-form] (kui/use-state form)]
    ($ :div
       (let [max-width (or (-> form :max-width)
                           :md)
             max-width (if (keyword? max-width)
                         (name max-width)
                         max-width)]
         ($ mui/Dialog
            {:open      open?
             :className @DIALOG-CLASS
             :fullWidth true
             :maxWidth max-width
            ;; :onClose close
             }

          ;; ($ :pre (-> open? u/->edn))

            (when-let [title (-> form :title)]
              ($ mui/DialogTitle
                 title))

            ($ mui/DialogContent
               ($ Form {:form form
                        :set-form set-form
                        :on-close on-close})))))))

(defnc FormDialogsContainer []
  (let [forms (use-dialog-forms)]
    (for [form (-> forms vals)]
      ($ FormDialog
         {:key (-> form :id)
          :form form}))))

;; TODO deprecated
(defnc FormCardArea [{:keys [form children]}]
  ($ mui/CardActionArea
     {:onClick #(show-form-dialog form)}
     children))

(defnc FieldLabel [{:keys [text]}]
  ($ :div
     {:class "FieldLabel"
      :style {:color "grey"}}
   ;; TODO (->component text)
     (str text)))

(defnc Field [{:keys [label description children]}]
  ($ :div
     {:class "Field EditableField"
      :style {:display "grid"
              :grid-gap "8px"}}
     ($ FieldLabel
        {:text label})
     ($ :div
        {:class "FieldValue"
         :style {:min-height "15px"}}
        children)
     (when description
       ($ :div description))))

(defnc StringVectorChips [{:keys [values]}]
  ($ :div
     {:style {:display :flex
              :flex-wrap :wrap
              :gap "8px"}}
     (for [value values]
       ($ :div
          {:key value}
          ($ mui/Chip
             {:label value})))))

(defnc FieldCardArea [{:keys [entity update-f field]}]
  #_(u/log-deprecated "use CommandCardArea")
  (u/assert-malli [:map] entity)
  (let [id     (form/field-id field)
        label  (or (when (spark/field-schema? field)
                     (-> field spark/schema-opts :label))
                   (get field :label))
        value  (get entity id)
        submit #(let [changes {id (get % id)}]
                  (if update-f
                    (update-f changes)
                    (db/update> entity %)))]
    ($ FormCardArea
       {:form {:fields [field]
               :values {id value}
               :submit submit}}
       ($ mui/CardContent
          ($ Field
             {:label label}
             (str value))))))

(defnc FieldsCardAreas [{:keys [entity update-f fields]}]
  (u/assert-malli [:map] entity)
  (s/assert map? entity)
  (s/assert ::form/fields fields)
  (<> (for [field fields]
        ($ FieldCardArea
           {:key (form/field-id field)
            :entity entity
            :update-f update-f
            :field field}))))

(defnc FieldsCard [{:keys [entity update-f fields children]}]
  (s/assert map? entity)
  (s/assert ::form/fields fields)
  ($ mui/Card
     ($ FieldsCardAreas
        {:entity entity
         :update-f update-f
         :fields fields})
     children))

;;;
;;; doc fields
;;; TODO deprecated: get rid of binding to docs
;;e

(defnc DocFieldCardArea [{:keys [doc doc-path field value-filter]}]
  (let [field        (if (spark/field-schema? field)
                       (spark/schema-opts field)
                       field)
        value-filter (or value-filter str)
        id           (form/field-id field)
        label        (get field :label)
        value        (get doc id)
        submit       #(let [changes {id (get % id)}]
                        (if doc
                          (db/update> doc changes)
                          (do
                            (u/log-deprecated "use doc, not doc-path")
                            (db/update> doc-path changes))))
        type         (get field :type)]
    ($ FormCardArea
       {:form {:fields [(assoc field :value value)]
               :submit submit}}
       ($ mui/CardContent
          ($ Field
             {:label label}
             (case type
               "chips" ($ StringVectorChips {:values value})
               ($ :div
                  {:style {:word-break "break-word"}}
                  (value-filter value))))))))

(defnc DocFieldsCardAreas [{:keys [doc fields]}]
  (<> (for [field fields]
        ($ DocFieldCardArea
           {:key (form/field-id field)
            :doc doc
            :field field}))))

(defnc DocFieldCard [{:keys [doc field value-filter]}]
  ($ mui/Card
     ($ DocFieldCardArea
        {:doc doc
         :field field
         :value-filter value-filter})))

(defnc DocFieldsCard [{:keys [doc fields title children]}]
  ($ mui/Card
     (when title
       ($ mui/CardContent
          ($ mui/Typography
             {:variant "overline"}
             title)))
     ($ DocFieldsCardAreas
        {:doc doc
         :fields fields})
     children))

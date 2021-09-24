(ns spark.form-ui
  (:require
   [clojure.spec.alpha :as s]
   [cljs.pprint :refer [pprint]]
   [cljs-bean.core :as cljs-bean]

   [helix.core :refer [defnc $ <>]]
   [helix.hooks :as hooks]
   [helix.dom :as d]


   ["@material-ui/core" :as mui]
   ["@material-ui/lab" :as mui-lab]

   [spark.react :as r]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.logging :refer [log]]
   ;; [spark.mui :as ui]
   [spark.form :as form]
   [spark.react :as react]
   [spark.db :as db]
   ))


(defonce DIALOG_FORMS (atom {}))

(defn close-form-dialog [form-id]
  (swap! DIALOG_FORMS assoc-in [form-id :open?] false)
  (js/setTimeout #(swap! DIALOG_FORMS dissoc form-id) 1000))


(defn show-form-dialog [form]
  (let [form-id (random-uuid)
        form    (form/initialize form)
        form    (assoc form
                       :open? true
                       :id form-id)]
    (swap! DIALOG_FORMS assoc form-id form)))

(defn show-form-dialog> [form]
  (u/promise>
   (fn [resolve reject]
     (show-form-dialog (assoc form
                              :then resolve
                              :catch reject)))))

(def use-dialog-forms (react/atom-hook DIALOG_FORMS))

(defonce HIDE_DIALOG (r/create-context nil))

(defn use-hide-form-dialog []
  (r/use-context HIDE_DIALOG))

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
  (let [inc-dec (fn [amount]
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
                            #_(when (-> field :plusminus-adronments)
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
                          (when (-> field :plusminus-adronments)
                            ($ :div
                               {:style {:display "grid"
                                        :grid-template-columns "max-content max-content"
                                        :grid-gap "8px"}}
                               ($ mui/Button
                                  {:onClick #(inc-dec -1)
                                   :variant "contained"
                                   :size "small"
                                   :className "FormPlusMinusAdornmentButton"}
                                  "-")
                               ($ mui/Button
                                  {:onClick #(inc-dec 1)
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
        InputProps  {:startAdornment start-adornment
                     :endAdornment end-adornment}
        ]
    ($ :div
       ;; ($ :pre (str field))
       ;; (when goog.DEBUG
       ;;   ($ :div
       ;;      {:style {:padding          "8px"
       ;;               :background-color "black"
       ;;               :color            "#6F6"
       ;;               :font-family      "monospace"}}
       ;;      (u/->edn (:value field))))
       ($ mui/TextField
          {
           :id              (-> field :id name)
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
           :rows            (get field :rows (when (get field :multiline?) 5))
           :autoFocus       (-> field :auto-focus?)
           :inputProps      (clj->js input-props)
           :InputProps      (clj->js InputProps)
           :InputLabelProps #js {:shrink true}
           :margin          "dense"
           ;; :variant "filled"
           :variant         "outlined"
           :fullWidth       true}))))


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
  (create-input (assoc field
                       :type "text"
                       :input-type "date")))

(defmethod create-input "number" [field]
  (let [pattern (or (-> field :input-props :pattern)
                    "[0-9]*")]
    (create-input (assoc field
                         :type "text"
                         :input-type "number"
                         :input-props (assoc (-> field :input-props)
                                             :pattern pattern)))))

(defmethod create-input "select" [field]
  (let [html-id     (str "select_" (-> field :id) "_input")
        input-props (-> field :input-props
                        (assoc :id html-id)
                        (assoc :name (-> field :name)))
        value       (-> field :value)
        required?   (-> field :required?)
        label-id (str "select_" (-> field :id) "_label")]
    ($ :div
       ($ mui/FormControl
          {:fullWidth true
           :variant      "outlined"
           :margin       "dense"
           }
          ($ mui/InputLabel
             {:htmlFor html-id
              :shrink  true
              :id label-id
              }
             (-> field :label))
          ($ mui/Select
             {
              :label (-> field :label)
              :native       true
              :id           (-> field :id name)
              :labelId label-id
              :name         (-> field :name)
              :defaultValue value
              :inputProps   (clj->js input-props)
              :required     required?
              :onChange     #((:on-change field)
                              (-> % .-target .-value))
              :autoFocus    (-> field :auto-focus?)
              }
             (when (or (nil? value) (not required?))
               ($ :option {:value nil} ""))
             (for [option (-> field :options)]
               ($ :option
                  {:key   (-> option :value)
                   :value (-> option :value)}
                  (-> option :label)))))
       )))


(defmethod create-input "chips" [field]
  ($ mui-lab/Autocomplete
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
       {
        :id           (-> field :id name)
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


(defmethod create-input "checkboxes" [field]
  (log ::create-input
       :field field)
  (let [value (into #{} (-> field :value))]
    ($ mui/FormControl
       {:component "fieldset"}
       ($ mui/FormGroup
          ($ :div {:style {:margin-top "8px"}})
          ($ mui/FormLabel
             {:component "legend"}
             ($ :div
                {:style {:font-size "16px"}}
                (-> field :label)))
          (for [option (-> field :options)]
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
                                          ((-> field :on-change) value))})}))))))

(defmethod create-input "ui" [field]
  ($ :div
     ($ (-> field :component)
        {:field field})))

(defnc FormField [{:keys [field form on-submit update-form]}]
  (let [field-id (-> field :id)
        error    (form/field-error form field-id)
        Input    (d/div
                  (create-input
                   (assoc field
                          :form form
                          :error error
                          :on-submit on-submit
                          :on-change #(update-form
                                       form/on-field-value-change
                                       field-id %)))
                  (when-let [helptext (-> field :helptext)]
                    (d/div
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
         Input))))

(def DIALOG-CLASS (atom nil))

(defnc FormDialog [{:keys [form]}]
  (let [[form set-form] (hooks/use-state form)

        form-id      (-> form :id)
        form         (assoc form :update (fn [f]
                                           (set-form (f form))))
        update-form_ (fn [f & args]
                       (set-form (apply f (into [form] args))))

        set-waiting (fn [waiting?]
                      (update-form_ form/set-waiting waiting?))

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

        ;; form (assoc form :update update-form)
        close     (fn [result]
                    (update-form assoc :open? false)
                    (close-form-dialog form-id)
                    (when-let [then (get form :then)]
                      (then result)))
        on-submit (fn []
                    (let [form (form/on-submit form)]
                      (update-form (fn [_] form))
                      (when-not (form/contains-errors? form)
                        (let [values (form/values form)]
                          (log ::submit
                               :form form
                               :values values)
                          (when-let [submit (get form :submit)]
                            (set-waiting true)
                            (let [p (u/promise> (fn [resolve]
                                                  (resolve
                                                   (submit values))))]
                              (-> p
                                  (.then (fn [result]
                                           (close result)
                                           ))
                                  (.catch (fn [error]
                                            (update-form (fn [_]
                                                           (form/set-error form error))))))))))))]
    (r/provider
     {:context HIDE_DIALOG
      :value   close}
     (d/div
      (let [max-width (or (-> form :max-width)
                          :md)
            max-width (if (keyword? max-width)
                        (name max-width)
                        max-width)]
        ($ mui/Dialog
           {:open      (-> form :open? boolean)
            :className @DIALOG-CLASS
            :fullWidth true
            :maxWidth max-width
            ;; :onClose close
            }

           (when-let [title (-> form :title)]
             ($ mui/DialogTitle
                title))

           ($ mui/DialogContent
              (when goog.DEBUG
                (when-let [data (-> form :debug-data)]
                  ($ :div
                     {:style {:padding          "8px"
                              :background-color "black"
                              :color            "#6F6"
                              :font-family      "monospace"}}
                     (u/->edn data))))
              ;; (when goog.DEBUG
              ;;   ($ :div
              ;;      {:style {:padding          "8px"
              ;;               :background-color "black"
              ;;               :color            "#6F6"
              ;;               :font-family      "monospace"}}
              ;;      (u/->edn (-> form :values))))
              #_($ :pre (-> context keys str))
              ($ :div
                 {:style {
                          ;; :width     "500px"
                          ;; :max-width "100%"
                          }}

                 (for [field (get form :fields)]
                   ($ FormField
                      {:key         (-> field :id)
                       :field       field
                       :form        form
                       :on-submit   on-submit
                       :update-form update-form}))


                 (get form :content))
              ;; (ui/data form)
              )

           (when-let [error (-> form form/error)]
             ($ :div
                {:style {:margin "16px"
                         :padding "16px"
                         :background-color "red"
                         :color "white"
                         :font-weight 900
                         :border-radius "8px"}}
                (str error)))

           ($ :div
              {:style {:padding               "16px"
                       :display               "grid"
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
                 ($ mui/Button
                    {:onClick #(close nil)}
                    "Abbrechen")
                 ($ mui/Button
                    {:onClick on-submit
                     :variant "contained"
                     :color   "primary"
                     :disabled (-> form form/waiting?)}
                    "Ok")))

           ($ :div
              {:style {:min-height "4px"}}
              (when (-> form :waiting?)
                ($ mui/LinearProgress)))))))))


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
  (d/div
   {:class "FieldLabel"
    :style {:color "grey"}}
   text))

(defnc Field [{:keys [label children]}]
  ($ :div
     {:spacing 0.5
      :class   "Field EditableField"}
     ($ FieldLabel
        {:text label})
     (d/div
      {:class "FieldValue"
       :style {:min-height "15px"}}
      children)))


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
;;;


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

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

   [spark.utils :as u]
   [spark.core :as spark]
   [spark.logging :refer [log]]
   ;; [spark.mui :as ui]
   [spark.form :as form]
   [spark.react :as react]
   [spark.repository :as repository]
   [spark.firestore :as fs]
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
  ($ :div
     ;; ($ :pre (str field))
     ($ mui/TextField
        {
         :id           (-> field :id name)
         :name         (-> field :name)
         :autoComplete (-> field :auto-complete)
         :value        (or (-> field :value) "")
         :required     (-> field :required?)
         :error        (boolean (-> field :error))
         :helperText   (-> field :error)
         :onChange     #((:on-change field)
                         (-> % .-target .-value))
         :onKeyPress   (when-not (-> field :multiline?)
                         #(when (= "Enter" (-> ^js % .-nativeEvent .-code))
                            ((:on-submit field))))
         :label        (-> field :label)
         :type         (-> field :input-type)
         :multiline    (get field :multiline?)
         :rows         (get field :rows (when (get field :multiline?) 5))
         :autoFocus    (-> field :auto-focus?)
         :inputProps   (if-let [props (-> field :input-props)]
                         (clj->js props)
                         (clj->js {}))
         ;; :InputLabelProps #js {:shrink false}
         :margin       "dense"
         ;; :variant "filled"
         :variant      "outlined"
         :fullWidth    true})))


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
  (create-input (assoc field
                       :type "text"
                       :input-type "number")))

(defmethod create-input "select" [field]
  (let [html-id (str "select_" (-> field :id) "_input")
        input-props (-> field :input-props
                        (assoc :id html-id)
                        (assoc :name (-> field :name)))
        value (-> field :value)
        required? (-> field :required?)]
    ($ :div
       ;; ($ :pre (str field))
       ($ mui/FormControl
          {:fullWidth true}
          ($ mui/InputLabel
             {:htmlFor html-id}
             (-> field :label))
          ($ mui/Select
             {
              :native true
              :id (-> field :id name)
              :name (-> field :name)
              :defaultValue value
              :inputProps (clj->js input-props)
              :required required?
              :onChange #((:on-change field)
                          (-> % .-target .-value))
              :autoFocus (-> field :auto-focus?)
              :margin "dense"
              :variant "filled"
              :fullWidth true
              }
             (when (or (nil? value) (not required?))
               ($ :option {:value nil} ""))
             (for [option (-> field :options)]
               ($ :option
                  {:key (-> option :value)
                   :value (-> option :value)}
                  (-> option :label)))))
       #_($ mui/TextField
            {
             :id (-> field :id name)
             :name (-> field :name)
             :autoComplete (-> field :auto-complete)
             :defaultValue (-> field :value)
             :required (-> field :required?)
             :error (boolean (-> field :error))
             :helperText (-> field :error)
             :onChange #((:on-change field)
                         (-> % .-target .-value))
             :onKeyPress (when-not (-> field :multiline?)
                           #(when (= "Enter" (-> ^js % .-nativeEvent .-code))
                              ((:on-submit field))))
             :label (-> field :label)
             :type (-> field :input-type)
             :multiline (get field :multiline?)
             :rows (get field :rows (when (get field :multiline?) 5))
             :autoFocus (-> field :auto-focus?)
             :inputProps (if-let [props (-> field :input-props)]
                           (clj->js props)
                           (clj->js {}))
             :margin "dense"
             :fullWidth true}))))


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
  ($ mui/FormControl
     {:component "fieldset"}
     ($ mui/FormGroup
        ($ mui/FormLabel
           {:component "legend"}
           (-> field :label))
        (for [option (-> field :options)]
          ($ mui/FormControlLabel
             {:key     (-> option :value)
              :label   (or (-> option :label)
                           (-> option :value str))
              :control ($ mui/Checkbox
                          {:name     (-> option :value str)
                           :checked  (-> field :value (contains? (-> option :value)))
                           :onChange #(let [checked? (-> % .-target .-checked)
                                            value    (-> field :value)
                                            value    (if checked?
                                                       (conj value (-> option :value))
                                                       (disj value (-> option :value)))]
                                        ((-> field :on-change) value))})})))))

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
        close     (fn []
                    (update-form assoc :open? false)
                    (close-form-dialog form-id))
        on-submit (fn []
                    (let [form (form/on-submit form)]
                      (update-form (fn [_] form))
                      (when-not (form/contains-errors? form)
                        (let [values (form/values form)]
                          (log ::submit
                               :form form
                               :values values)
                          #_(when-let [command (get form :command)]
                              (-> (runtime/execute-command>
                                   command
                                   (assoc context
                                          :values values))
                                  (.then close)))
                          (when-let [submit (get form :submit)]
                            (let [result (submit values)
                                  p      (u/as> result)]
                              (u/=> p
                                    (fn [result]
                                      (close)
                                      (when-let [then (get form :then)]
                                        (then result))))))))))]
    (d/div
     ($ mui/Dialog
        {:open      (-> form :open? boolean)
         :className @DIALOG-CLASS
         ;; :onClose close
         }

        (when-let [title (-> form :title)]
          ($ mui/DialogTitle
             title))

        ($ mui/DialogContent
           #_($ :pre (-> context keys str))
           ($ :div
              {:style {:width     "500px"
                       :max-width "100%"}}

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
        ($ mui/DialogActions
           ($ mui/Button
              {:onClick close}
              "Abbrechen")
           ($ mui/Button
              {:onClick on-submit
               :variant "contained"
               :color   "primary"}
              "Ok"))
        ($ :div
           {:style {:min-height "4px"}}
           (when (-> form :waiting?)
             ($ mui/LinearProgress)))))))


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
   {:style {:color "grey"}}
   text))

(defnc Field [{:keys [label children]}]
  ($ :div
     {:spacing 0.5
      :class "EditableField"}
     ($ FieldLabel
        {:text label})
     (d/div
      {:style {:min-height "15px"}}
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
  (let [id (form/field-id field)
        label (or (when (spark/field-schema? field)
                    (-> field spark/schema-opts :label))
                  (get field :label))
        value (get entity id)
        submit #(let [changes {id (get % id)}]
                  (if update-f
                    (update-f changes)
                    (fs/update-fields> entity %)))]
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
  (let [field (if (spark/field-schema? field)
                (spark/schema-opts field)
                field)
        value-filter (or value-filter str)
        id (form/field-id field)
        label (get field :label)
        value (get doc id)
        submit #(let [changes {id (get % id)}]
                  (if doc
                    (repository/update-doc> doc changes)
                    (do
                      (u/log-deprecated "use doc, not doc-path")
                      (fs/update-fields> doc-path changes))))
        type (get field :type)]
    ($ FormCardArea
       {:form {:fields [(assoc field :value value)]
               :submit submit}}
       ($ mui/CardContent
          ($ Field
             {:label label}
             (case type
               "chips" ($ StringVectorChips {:values value})
               (value-filter value)))))))


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

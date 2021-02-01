(ns commons.form-ui
  (:require
   [clojure.spec.alpha :as s]
   [cljs.pprint :refer [pprint]]
   [cljs-bean.core :as cljs-bean]

   [helix.core :refer [defnc $ <>]]
   [helix.hooks :as hooks]
   [helix.dom :as d]

   ["react-router-dom" :as router]

   ["@material-ui/core" :as mui]
   ["@material-ui/core/styles" :as mui-styles]

   ["material-ui-chip-input" :default ChipInput]

   [commons.utils :as u]
   [commons.logging :refer [log]]
   [commons.context :as c.context]
   ;; [commons.mui :as ui]
   [commons.form :as form]
   [commons.context :as context]
   [commons.firestore :as fs]
   ))


(defonce DIALOG_FORMS (atom {}))

(defn close-form-dialog [form-id]
  (swap! DIALOG_FORMS assoc-in [form-id :open?] false)
  (js/setTimeout #(swap! DIALOG_FORMS dissoc form-id) 1000))

(defn show-form-dialog [form]
  (let [form-id (random-uuid)
        form (form/initialize form)
        form (assoc form
                    :open? true
                    :id form-id)]
    (swap! DIALOG_FORMS assoc form-id form)))

(def use-dialog-forms (context/atom-hook DIALOG_FORMS))

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
         :fullWidth true})))

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

(defmethod create-input "chips" [field]
  ($ ChipInput
     {
      :id (-> field :id name)
      :name (-> field :name)
      :defaultValue (clj->js (-> field :value))
      :onChange #((:on-change field) (-> % js->clj))
      :dataSource (clj->js ["hallo" "welt"])
      :label (-> field :label)
      :autoFocus (-> field :auto-focus?)
      :margin "dense"
      :fullWidth true}))

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

(defnc FormDialog [{:keys [form]}]
  (let [[form set-form] (hooks/use-state form)
        form-id (-> form :id)
        update-form (fn [f & args]
                      (set-form (apply f (into [form] args))))
        close (fn []
                (update-form assoc :open? false)
                (close-form-dialog form-id))
        on-submit (fn []
                    (let [form (form/on-submit form)]
                      (update-form (fn [_] form))
                      (when-not (form/contains-errors? form)
                        (let [values (form/values form)]
                          (log ::submit
                               :form form
                               :values values )
                          #_(when-let [command (get form :command)]
                            (-> (runtime/execute-command>
                                 command
                                 (assoc context
                                        :values values))
                                (.then close)))
                          (when-let [submit (get form :submit)]
                            (submit values )
                            (close))))))]
    (d/div
     ($ mui/Dialog
        {:open (-> form :open? boolean)
         ;; :onClose close
         }
        ($ mui/DialogContent
           #_($ :pre (-> context keys str))
           ($ :div
              {:style {:width "500px"
                       :max-width "100%"}}
              (for [field (get form :fields)]
                (let [field-id (-> field :id)
                      error (form/field-error form field-id)
                      Input (d/div
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
                             ($ mui/Button
                                {:onClick (fn [_]
                                            (update-form (-> action :f)))
                                 :variant "contained"}
                                (-> action :label))))
                       Input))))
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
               :color "primary"}
              "Ok"))))))


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
  (s/assert map? entity)
  (s/assert ::form/field field)
  (let [id (or (-> field :id)
               (-> field :attr/key))
        label (get field :label)
        value (get entity id)
        submit #(let [changes {id (get % id)}]
                  (if update-f
                    (update-f changes)
                    (fs/update-fields> entity %)))
        type (get field :type)]
    ($ FormCardArea
       {:form {:fields [(assoc field :value value)]
               :submit submit}}
       ($ mui/CardContent
          ($ Field
             {:label label}
             (case type
               "chips" ($ StringVectorChips {:values value})
               (str value)))))))

(defnc FieldsCardAreas [{:keys [entity update-f fields]}]
  (s/assert map? entity)
  (s/assert ::form/fields fields)
  (<> (for [field fields]
        ($ FieldCardArea
           {:key (or (-> field :id)
                     (-> field :attr/key))
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
  (let [value-filter (or value-filter str)
        id (or (get field :id)
               (get field :attr/key))
        label (get field :label)
        value (get doc id)
        submit #(let [changes {id (get % id)}]
                  (fs/update-fields> (or doc doc-path) changes))
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
           {:key (or (-> field :id)
                     (-> field :model/id))
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

(ns spark.form
  (:require
   [clojure.spec.alpha :as s]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.money :as money]
   [spark.local :as local]


   [clojure.string :as str]
   [clojure.set :as set]))

(s/def ::id keyword?)
(s/def ::submit fn?)
(s/def ::field (s/or :id (s/keys :req-un [::id])
                     :attr (s/keys :req [:attr/key])
                     :field vector?
                     :nil nil?))
(s/def ::fields (s/coll-of ::field
                           :min-count 1))
(s/def ::form (s/keys :req-un [::fields ::submit]))


(defn field-id [field]
  (if (spark/field-schema? field)
    (-> field spark/schema-opts :id)
    (or (-> field :field-id)
        (-> field :id)
        (-> field :attr/key))))

(defn- initialize-field [form idx field]
  (let [field         (if (spark/field-schema? field)
                        (spark/field-schema-as-form-field field)
                        field)
        id            (-> field field-id)
        values        (-> form :values)
        fields-values (-> form :fields-values)
        field-type    (let [type (-> field :type)]
                        (cond
                          (nil? type)    :text
                          (string? type) (keyword type)
                          :else          type))
        value         (or (get values id)
                          (get field :value)
                          (get fields-values id)
                          (get field :default-value))
        value         (case field-type
                        :checkboxes (into #{} value)
                        :chips      (if (-> field :sort?)
                                      (sort value)
                                      value)
                        value)
        field         (if value
                        (assoc field :value value)
                        field)
        field-name    (or (-> field :name)
                          (when-let [id (-> field :id)]
                            (if (keyword? id)
                              (name id)
                              (str id))))

        options (when-let [options (-> field :options)]
                  (->> options
                       (mapv (fn [option]
                               (if (map? option)
                                 option
                                 {:value option
                                  :label (str option)})))))
        ]
    (assoc field
           :id id
           :auto-focus? (= 0 idx)
           :name field-name
           :label (or (-> field :label)
                      (-> field :name)
                      field-name)
           :type field-type
           :multiline? (or (-> field :multiline?)
                           (-> field :rows boolean))
           :auto-complete (get field :auto-complete "off")
           :options options)))

(def ^js eur-number-format
  (-> js/Intl (.NumberFormat "de-DE"
                             (clj->js {:style    "currency"
                                       :currency "EUR"}))))

(defn- format-eur [value]
  (when value
    (-> eur-number-format
        (.format value)
        (.replace "€" "")
        .trim)))

(defn- prepare-field-value [value field]
  (case (-> field :type)

    :eur (format-eur value)

    :checkboxes
    (->> field :options (map :value) (into #{}) (set/intersection value))

    value))

(comment
  (prepare-field-value
   #{:a :b :c}
   {:type :checkboxes
    :options [{:value :b}]}))

(defn- spy-form-values [form]
  (log ::spy-form-values
       :values (-> form :values))
  form)


(defn initialize [form]
  (s/assert ::form form)
  ;; (log ::initialize
  ;;      :form form)
  (let [fields (into []
                     (map-indexed
                      (partial initialize-field form)
                      (->> form
                           :fields
                           (remove nil?))))
        form   (assoc form :fields fields)
        form   (assoc form :values
                      (reduce (fn [values field]
                                (let [field-id (-> field field-id)
                                      value    (or (-> values (get field-id))
                                                   (-> field :value)
                                                   (-> field :default-value))
                                      value    (prepare-field-value value field)]
                                  (assoc values field-id value)))
                              (or (-> form :values) {}) (-> form :fields)))
        form   (update form :fields
                       (fn [fields]
                         (mapv (fn [field]
                                 (assoc field :value (get-in form [:values (:id field)])))
                               fields)))]
    (log ::initialized
         :form form)
    form))


;; (defn load-values [form values-map]
;;   ;; (log ::load-values
;;   ;;      :form form
;;   ;;      :values values-map)
;;   (s/assert ::fields (-> form :fields))
;;   (update form :fields
;;           #(mapv (fn [field]
;;                    (assoc field :value (get values-map (-> field :id))))
;;                  %)))


(defn field-by-id [form field-id]
  (s/assert ::form form)
  (some #(when (= field-id (-> % :id)) %)
        (-> form :fields)))

(defn field-index-by-id [form field-id]
  (->> form
       :fields
       (map-indexed vector)
       (some #(when (= field-id (-> % second :id)) (first %)))))

(defn field-type [form field-id]
  (-> form
      (field-by-id field-id)
      :type
      (or "text")))

(defn default-number-validator [form field]
  (fn [value form]
    (let [min-value (-> field :min)
          max-value (-> field :max)]
      (when value
        (cond
          (and min-value (< value min-value)) (str "Minimum: " min-value)
          (and max-value (> value max-value)) (str "Maximum: " max-value))))))

(defn default-eur-validator [form field]
  (fn [value form]
    (let [min-value (-> field :min)
          max-value (-> field :max)]
      (when value
        (cond
          (and min-value (money/< value min-value)) (str "Minimum: " (local/format-eur min-value))
          (and max-value (money/> value max-value)) (str "Maximum: " (local/format-eur max-value)))))))

(defn default-field-validator [form field-id]
  (let [field (field-by-id form field-id)]
    (case (-> field :type)
      :number (default-number-validator form field)
      :int (default-number-validator form field)
      :eur (default-eur-validator form field)
      nil)))

(defn field-validator [form field-id]
  (or (-> form
          (field-by-id field-id)
          :validator)
      (default-field-validator form field-id)))

(defn field-coercer [form field-id]
  (-> form
      (field-by-id field-id)
      :coercer))

(defn values [form]
  (-> form :values))


(defn field-value [form field-id]
  (let [values (values form)]
    (or (get values field-id)
        (-> (field-by-id form field-id) :value))))


(defn coerce-value [value form field-id]
  (let [field-type (field-type form field-id)
        value      (if (string? value) (str/trim value) value)]
    ;; (log ::coerce-value
    ;;      :field-id field-id
    ;;      :type field-type
    ;;      :value value)
    (if (or (nil? value)
            (= "" value))
      nil
      (let [value   (case field-type
                      :number (js/parseInt value)
                      :int (js/parseInt value)
                      :eur (-> value (.replace "," "."))
                      value)
            coercer (field-coercer form field-id)
            value   (if-not coercer
                      value
                      (coercer value form))]
        value))))

(defn coerce-values [form]
  (->> form :fields (map :id)
       (reduce (fn [form field-id]
                 (let [value (coerce-value (get-in form [:values field-id])
                                           form field-id)]
                   (assoc-in form [:values field-id] value)))
               form)))

(defn validate-field [form field-id]
  (let [field     (field-by-id form field-id)
        value     (field-value form field-id)
        value     (coerce-value value form field-id)
        error     (when (and  (-> field :required?) (nil? value))
                    (local/text :form-field-input-required))
        validator (field-validator form field-id)
        error     (or error
                      (when (and value validator)
                        (try
                          (validator value form)
                          (catch :default ex
                            (log ::field-validator-failed
                                 :field field
                                 :form form)
                            (str ex)))))]
    (if error
      (assoc-in form [:errors field-id] error)
      (update form :errors dissoc field-id))))

(defn set-field-attr [form field-id attr value]
  (let [field-index (field-index-by-id form field-id)]
    (-> form
        (assoc-in [:fields field-index attr] value)
        (validate-field field-id))))

(defn set-field-value [form field-id value]
  (let [field-index (field-index-by-id form field-id)]
    (-> form
        (assoc-in [:values field-id] value)
        (assoc-in [:fields field-index :value] value)
        (validate-field field-id))))

(defn set-fields-values [form values]
  (reduce (fn [form [field-id value]]
            (set-field-value form field-id value))
          form values))

(defn on-field-value-change [form field-id new-value]
  ;; (log ::on-field-value-change
  ;;      :values (-> form :values)
  ;;      :field field-id
  ;;      :value new-value)
  (let [form (-> form
                 (set-field-value field-id new-value))
        field (field-by-id form field-id)
        on-change (-> field :on-change)
        form (if-not on-change
               form
               (on-change form new-value))]
    form))

(defn field-error [form field-id]
  (get-in form [:errors field-id]))

(defn error [form]
  (-> form :error))

(defn set-error [form error]
  (assoc form :error error))

(defn contains-errors? [form]
  (boolean
   (or (-> form error)
       (-> form :errors seq))))

(defn on-submit [form]
  (s/assert ::form form)
  ;; (log ::on-submit
  ;;      :form form)
  (let [form (assoc form :error nil)]
    (->> form
         :fields
         (map :id)
         (reduce validate-field form)
         coerce-values)))

(comment
  (on-submit {:fields [{:id :f1}]
              :submit (fn [])}))

(defn set-waiting [form value]
  (assoc form :waiting? (boolean value)))

(defn waiting? [form]
  (-> form :waiting?))

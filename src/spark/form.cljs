(ns spark.form
  (:require
   [clojure.spec.alpha :as s]
   [spark.logging :refer [log]]
   [spark.models :as models]

   [clojure.string :as str]))

(s/def ::id keyword?)
(s/def ::submit fn?)
(s/def ::field (s/or :id (s/keys :req-un [::id])
                     :attr (s/keys :req [:attr/key])))
(s/def ::fields (s/coll-of ::field
                           :min-count 1))
(s/def ::form (s/keys :req-un [::fields ::submit]))


(defn field-id [field]
  (or (-> field :id)
      (-> field :attr/key)))


(defn- initialize-field [form idx field ]
  (let [id (-> field field-id)
        values (-> form :values)
        fields-values (-> form :fields-values)
        field (if (models/attr? field)
                (models/attr->form-field field)
                field)
        value (or (get values id)
                  (get field :value)
                  (get fields-values id)
                  (get field :default-value))
        field (if value
                (assoc field :value value)
                field)]
    (assoc field
           :auto-focus? (= 0 idx)
           :name (or (-> field :name)
                     (-> field :id name))
           :label (or (-> field :label)
                      (-> field :name)
                      (-> field :id name))
           :multiline? (or (-> field :multiline?)
                           (-> field :rows boolean))
           :auto-complete (get field :auto-complete "off"))))

(defn- spy-form-values [form]
  (log ::spy-form-values
       :values (-> form :values))
  form)

(defn initialize [form]
  (s/assert ::form form)
  (log ::initialize
       :form form)
  (let [form (assoc form :fields (map-indexed  (partial initialize-field form)
                                          (-> form :fields)))]
    (-> form
        (assoc :values
               (reduce (fn [values field]
                         (let [field-id (-> field field-id)]
                           (if (get values field-id)
                             values
                             (let [value (or (-> field :value)
                                             (-> field :default-value))]
                               (if value
                                 (assoc values field-id value)
                                 values)))))
                       (or (-> form :values) {}) (-> form :fields)))
        )))


(defn load-values [form values-map]
  (s/assert ::fields (-> form :fields))
  (update form :fields
          #(mapv (fn [field]
                   (assoc field :value (get values-map (-> field :id))))
                 %)))


(defn field-by-id [form field-id]
  (s/assert ::form form)
  (some #(when (= field-id (-> % :id)) %)
        (-> form :fields)))


(defn field-type [form field-id]
  (-> form
      (field-by-id field-id)
      :type
      (or "text")))


(defn values [form]
  (-> form :values))


(defn field-value [form field-id]
  (let [values (values form)]
    (or (get values field-id)
        (-> (field-by-id form field-id) :value))))


(defn validate-field [form field-id]
  (let [field (field-by-id form field-id)
        value (field-value form field-id)
        error (when (and  (-> field :required?) (nil? value))
                "Input required.")]
    (if error
      (assoc-in form [:errors field-id] error)
      (update form :errors dissoc field-id))))


(defn adopt-value [value form field-id]
  (let [value (if (string? value) (str/trim value) value)]
    (if (or (nil? value)
            (= "" value))
      nil
      (case (field-type form field-id)
        "number" (js/parseInt value)
        value))))


(defn on-field-value-change [form field-id new-value]
  ;; (log ::on-field-value-change
  ;;      :values (-> form :values)
  ;;      :field field-id
  ;;      :value new-value)
  (-> form
      (assoc-in [:values field-id]
                (adopt-value new-value form field-id))
      (validate-field field-id)))


(defn contains-errors? [form]
  (-> form :errors seq boolean))


(defn field-error [form field-id]
  (get-in form [:errors field-id]))


(defn on-submit [form]
  (s/assert ::form form)
  ;; (log ::on-submit
  ;;      :form form)
  (reduce validate-field
          form (->> form :fields (map :id))))

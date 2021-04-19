(ns spark.form
  (:require
   [clojure.spec.alpha :as s]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]


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
    (or (-> field :id)
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
                              (str id))))]
    (assoc field
           :auto-focus? (= 0 idx)
           :name field-name
           :label (or (-> field :label)
                      (-> field :name)
                      field-name)
           :type field-type
           :multiline? (or (-> field :multiline?)
                           (-> field :rows boolean))
           :auto-complete (get field :auto-complete "off"))))

(defn- prepare-field-value [value field]
  (case (-> field :type)

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
  (log ::initialize
       :form form)
  (let [fields (into []
                     (map-indexed
                      (partial initialize-field form)
                      (->> form
                           :fields
                           (remove nil?))))
        form (assoc form :fields fields)
        form (assoc form :values
                    (reduce (fn [values field]
                              (let [field-id (-> field field-id)
                                    value (or (-> values (get field-id))
                                              (-> field :value)
                                              (-> field :default-value))
                                    value (prepare-field-value value field)]
                                (assoc values field-id value)))
                            (or (-> form :values) {}) (-> form :fields)))
        form (update form :fields
                     (fn [fields]
                       (mapv (fn [field]
                               (assoc field :value (get-in form [:values (:id field)])))
                             fields)))]
    ;; (log ::initialized
    ;;      :form form)
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


(defn values [form]
  (-> form :values))


(defn field-value [form field-id]
  (let [values (values form)]
    (or (get values field-id)
        (-> (field-by-id form field-id) :value))))


(defn adopt-value [value form field-id]
  (let [field-type (field-type form field-id)
        value      (if (string? value) (str/trim value) value)]
    ;; (log ::adopt-value
    ;;      :field-id field-id
    ;;      :type field-type
    ;;      :value value)
    (if (or (nil? value)
            (= "" value))
      nil
      (case field-type
        :number (js/parseInt value)
        ;; :chips  (into #{} value)
        value))))

(defn adopt-values [form]
  (->> form :fields (map :id)
       (reduce (fn [form field-id]
                 (let [value (adopt-value (get-in form [:values field-id])
                                          form field-id)]
                   (assoc-in form [:values field-id] value)))
               form)))

(defn validate-field [form field-id]
  (let [field (field-by-id form field-id)
        value (field-value form field-id)
        value (adopt-value value form field-id)
        error (when (and  (-> field :required?) (nil? value))
                "Input required.")]
    (if error
      (assoc-in form [:errors field-id] error)
      (update form :errors dissoc field-id))))

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
  (log ::on-field-value-change
       :values (-> form :values)
       :field field-id
       :value new-value)
  (-> form
      (set-field-value field-id new-value)))


(defn contains-errors? [form]
  (-> form :errors seq boolean))


(defn field-error [form field-id]
  (get-in form [:errors field-id]))


(defn on-submit [form]
  (s/assert ::form form)
  ;; (log ::on-submit
  ;;      :form form)
  (->> form :fields (map :id)
       (reduce validate-field form)
       adopt-values))

(defn set-waiting [form value]
  (assoc form :waiting? (boolean value)))

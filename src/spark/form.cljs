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

(defn- initialize-option [option mapper]
  (if (map? option)
    option
    {:value option
     :label (if mapper
              (mapper option)
              (str option))}))

(defn- initialize-field-options [field form]
  (if-not (or (-> field :options)
              (-> field :keytable))
    field
    (let [mapper (-> field :option-mapper)
          options (or (-> field :keytable vals)
                      (when-let [options (-> field :options)]
                        (->> options (mapv #(initialize-option % mapper)))))]
      (-> field
          (assoc :options options)))))

(defn- initialize-field [field form idx]
  (let [field         (if (spark/field-schema? field)
                        (spark/field-schema-as-form-field field)
                        field)
        id            (-> field field-id)
        values        (-> form :values)
        fields-values (-> form :fields-values)
        field-type    (let [type (-> field :type)]
                        (cond
                          (nil? type)    (or (when (-> field :keytable) :select)
                                             :text)
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

        auto-focus-first-field? (get form :auto-focus-first-field? true)]
    (-> field
        (assoc :id id
               :auto-focus? (boolean (and auto-focus-first-field?
                                          (= 0 idx)))
               :name field-name
               :label (or (-> field :label)
                          (-> field :name)
                          field-name)
               :type field-type
               :multiline? (or (-> field :multiline?)
                               (-> field :rows boolean))
               :auto-complete (get field :auto-complete "off"))
        (initialize-field-options form))))

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

(defn prepare-field-value--text [value field]
  ;; (tap> [:prepare-field-value--text value (sequential? value) (-> field :multiline?)])
  (cond
    (nil? value) nil
    (string? value) value

    (or (set? value) (sequential? value))
    (if (-> field :multiline?)
      (->> value (map str) (str/join "\n"))
      (->> value (map str) (str/join " ")))

    :else (str value)))

(comment
  (seq? [:a])
  (seq [:a])
  (sequential? #{})
  (set? #{})
  (sequential? [])
  (sequential? "abc"))

(defn- prepare-field-value [value field]
  ;; (tap> [:prepare-field-value value field])
  (case (-> field :type)

    :eur (format-eur value)

    :checkboxes
    (->> field :options (map :value) (into #{}) (set/intersection value))

    :text
    (prepare-field-value--text value field)

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
  (let [form-id (or (-> form :id)
                    (u/nano-id))
        form (assoc form :id form-id)
        fields (->> form :fields
                    (remove nil?)
                    (map-indexed vector)
                    (mapv (fn [[idx field]]
                            (initialize-field field form idx))))
        form   (assoc form :fields fields)
        form   (assoc form :values
                      (reduce (fn [values field]
                                (let [field-id (-> field field-id)
                                      value    (or (-> values (get field-id))
                                                   (-> field :value)
                                                   (-> field :default-value))
                                      value    (prepare-field-value value field)]
                                  (assoc values field-id value)))
                              (or (-> form :values) {}) fields))
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

(defn default-email-validator [form field]
  (fn [value form]
    (when value
      (when-not (and (-> value (str/includes? "@"))
                     (-> value (str/includes? "."))
                     (-> value (str/includes? " ") not))
        (local/text :invalid-input)))))

(defn default-field-validator [form field-id]
  (let [field (field-by-id form field-id)]
    (case (-> field :type)
      :number (default-number-validator form field)
      :int (default-number-validator form field)
      :eur (default-eur-validator form field)
      :email (default-email-validator form field)
      nil)))

(defn field-validator [form field-id]
  (or (-> form
          (field-by-id field-id)
          :validator)
      (default-field-validator form field-id)))

(defn field-coercer [form field-id]
  (when-let [coercer (-> form (field-by-id field-id) :coercer)]
    (if (fn? coercer)
      coercer
      (case coercer
        :lines
        (fn [value _form]
          (when value
            (->> (str/split-lines value)
                 (map str/trim)
                 (remove str/blank?))))

        :ints
        (fn [value _form]
          (when value
            (->> (str/split value #"[\s,]")
                 (map js/parseInt))))

        :sorted-ints
        (fn [value _form]
          (when value
            (->> (str/split value #"[\s,]")
                 (map js/parseInt)
                 sort)))))))

(defn values [form]
  (-> form :values))

(defn field-value-internal [form field-id]
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

(defn validate-field [form field-id]
  (let [field     (field-by-id form field-id)
        ;; value     (field-value-internal form field-id)
        ;; value     (coerce-value value form field-id)
        value (get-in form [:values field-id])
        error     (when (and  (-> field :required?)
                              (if (-> field :type (= :checkbox))
                                (not value)
                                (nil? value)))
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
    (log ::validate-field
         :field-id field-id
         :type (-> field :type)
         :value value
         :error error)
    (if error
      (assoc-in form [:errors field-id] error)
      (update form :errors dissoc field-id))))

(defn set-field-attr [form field-id attr value]
  (let [field-index (field-index-by-id form field-id)]
    (-> form
        (assoc-in [:fields field-index attr] value)
        (validate-field field-id))))

(defn set-field-value [form field-id value]
  (let [field-index (field-index-by-id form field-id)
        field (field-by-id form field-id)]
    (-> form
        (assoc-in [:values field-id] value)
        (assoc-in [:fields field-index :value] (prepare-field-value value field))
        (validate-field field-id))))

(defn set-fields-values [form values]
  (reduce (fn [form [field-id value]]
            (set-field-value form field-id value))
          form values))

(defn on-field-value-change [form field-id new-value-internal]
  ;; (log ::on-field-value-change
  ;;      :values (-> form :values)
  ;;      :field field-id
  ;;      :value new-value)
  ;; TODO Optimization: call on-change only if external value changed
  (let [new-value-external (coerce-value new-value-internal form field-id)
        ;; _ (tap> {:id field-id
        ;;          :new new-value-internal
        ;;          :coerced new-value-external})
        field-index (field-index-by-id form field-id)
        form (-> form
                 (assoc-in [:fields field-index :value] new-value-internal)
                 (assoc-in [:values field-id] new-value-external)
                 (validate-field field-id))

        field (field-by-id form field-id)
        field-on-change (-> field :on-change)
        form (if-not field-on-change
               form
               (field-on-change form new-value-external))
        form-on-change (-> form :on-change)
        form (if-not form-on-change
               form
               (form-on-change form field-id new-value-external))]
    ;; (tap> {:in-form (get-in form [:values field-id])})
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

(defn- validate-form [form]
  (if-let [f (-> form :validate)]
    (if-let [error (f (values form))]
      (assoc form :error error)
      form)
    form))

(defn on-submit [form]
  (s/assert ::form form)
  ;; (log ::on-submit
  ;;      :form form)
  (let [form (assoc form :error nil)
        form (->> form
                  :fields
                  (map :id)
                  (reduce validate-field form))
        form (if (-> form :error)
               form
               (validate-form form))]
    form))

(comment
  (on-submit {:fields [{:id :f1}]
              :submit (fn [])}))

(defn set-waiting [form value]
  (assoc form :waiting? (boolean value)))

(defn waiting? [form]
  (-> form :waiting?))

(defn set-submitted [form]
  (-> form
      (set-waiting false)
      (assoc :submitted? true)))

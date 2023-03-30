(ns spark.money
  (:refer-clojure :exclude [pos? zero? min max + < > <= >=])
  (:require
   ["dinero.js" :as dinero]
   [spark.utils :as u]))

;; https://dinerojs.com/

(comment
  (def m (dinero (clj->js {:amount 23 :currency "EUR"})))
  (def m (dinero (clj->js {:amount 2300 :currency "EUR"})))
  (def m (dinero (clj->js {:amount 10023 :currency "EUR"})))
  (type m)
  (-> m .toObject)
  (-> m .hasCents))

(defonce DEFAULT_CURRENCY (atom "EUR"))

(defn default-currency []
  @DEFAULT_CURRENCY)

(defn money [v]
  (when v
    (cond

      (int? v)
      (money {:amount (* v 100)})

      (string? v)
      (let [f (u/parse-float v)]
        (when (js/isNaN f)
          (throw (ex-info (str "Value is not a number: " v)
                          {:v v})))
        (money {:amount (-> f
                            (* 100)
                            (js/Math.round 2))}))

      (map? v)
      (let [amount   (or (-> v :amount) 0)
            currency (or (-> v :currency)
                         (default-currency))]
        (dinero (clj->js {:amount   amount
                          :currency currency})))

      (.hasOwnProperty v "toUnit")
      v

      :else
      (throw (ex-info (str "Unsupported money value: " v)
                      {:v v})))))

(comment
  (.toObject (money "20"))
  (.toObject (money "2.30"))

  (.toObject (money {:amount 2200}))
  (.toObject (money 0))
  (.toObject (money "22.11"))
  (.toObject (money (money "22.11")))
  (.toObject (money "22.22x"))
  (.toObject (money "c22.22x"))

  (-> (money "23.42") .getAmount (/ 100) type))

(defn money? [v]
  (and v
       (try
         (boolean (money v))
         (catch :default _ex
           false))))

(comment
  (money? "b")
  (money? "22")
  (money? "22c")
  (money? "c22c"))

(defn zero? [m]
  (if m
    (-> ^js (money m) .isZero)
    true))

(defn pos? [m]
  (if m
    (let [m ^js (money m)]
      (if (-> m .isZero)
        false
        (-> m .isPositive)))
    false))

(defn pos-or-zero? [m]
  (or (pos? m)
      (zero? m)))

(comment
  (pos? "23")
  (pos? "-23")
  (pos? "0")
  (pos? nil))

(defn pos-or-nil [m]
  (when (pos? m) m))

(defn ->str [m]
  (cond
    (nil? m)
    nil

    (string? m)
    m

    :else
    (let [s (-> ^js m .toUnit str)
          dot-idx (-> s (.indexOf "."))]
      (if (clojure.core/>= dot-idx 0)
        (if (-> s (.substring dot-idx) count (= 2))
          (str s "0")
          s)
        (str s ".00")))))

(comment
  (-> (money "20")
      (.subtract (money "2.30"))
      (->str))
  (-> (money "2")  ->str)
  (-> (money "2.1")  ->str)
  (-> (money "2.12")  ->str))

(defn ->cents [m]
  (cond
    (nil? m)
    nil

    (money? m)
    (-> ^js (money m) .getAmount)

    :else
    (->cents (money m))))

(defn ->number [m]
  (when-let [cents (->cents m)]
    (-> cents (/ 100) )))

(comment
  (->number "23.42"))

(defn ->number-string [m]
  (when-let [cents (->cents m)]
    (-> cents (/ 100) (.toFixed 2))))

(defn multiply [m factor]
  (when m
    (-> (money m)
        (.multiply factor "HALF_UP"))))

(comment
  (->str (multiply "2.50" 4)))

(defn subtract [m1 m2]
  (cond
    (and m1 m2) (-> ^js (money m1) ^js (.subtract ^js (money m2)))
    m1 m1
    m2 m2))

(comment
  (->str (subtract "20" "2.30"))
  (->str (subtract "10" 1))
  (->str (subtract "10" nil))
  (->str (subtract nil 2)))

(defn add [m1 m2]
  (cond
    (and m1 m2) (-> (money m1) (.add (money m2)))
    m1 m1
    m2 m2))

(defn sum [vals]
  (reduce (fn [ret val]
            (if val
              (-> ret (.add (money val)))
              ret))
          (money 0) vals))

(comment
  (->str (sum [2 "3.20" nil])))

(defn + [& vals]
  (reduce (fn [ret val]
            (cond
              (and (nil? ret) (nil? val)) nil
              (nil? ret) (money val)
              (nil? val) ret
              :else (-> ret (.add (money val)))))
          nil vals))

(comment
  (->str (+ 2 "3.20" nil)))

(defn min [vals]
  (->> vals
       (remove nil?)
       (map money)
       (.minimum dinero)))

(comment
  (->str (min [2 "-4"]))
  (->str (min [2 "4" nil])))

(defn max [vals]
  (->> vals
       (remove nil?)
       (map money)
       (.maximum dinero)))

(defn > [& vals]
  (->> vals
       (map ->number)
       (apply clojure.core/>)))

(comment
  (> "10" "5")
  (> "5" "10")
  (> "1.2" "1.1" "1"))

(defn < [& vals]
  (->> vals
       (map ->number)
       (apply clojure.core/<)))

(defn >= [& vals]
  (->> vals
       (map ->number)
       (apply clojure.core/>=)))

(defn <= [& vals]
  (->> vals
       (map ->number)
       (apply clojure.core/<=)))

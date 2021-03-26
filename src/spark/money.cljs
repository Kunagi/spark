(ns spark.money
  (:require
   ["dinero.js" :as dinero]))

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
      (money {:amount (-> (js/parseFloat v) (* 100) int)})

      (map? v)
      (let [amount (or (-> v :amount) 0)
            currency (or (-> v :currency)
                         (default-currency))]
        (js/console.log amount currency)
        (dinero (clj->js {:amount amount
                          :currency currency})))

      (.hasOwnProperty v "toUnit")
      v

      :else
      (throw (ex-info (str "Unsupported money value: " v)
                      {:v v})))))

(comment
  (.toObject (money {:amount 2200}))
  (.toObject (money 0))
  (.toObject (money "22.11"))
  (.toObject (money (money "22.11"))))


(defn ->str [m]
  (cond
    (nil? m)
    nil

    (string? m)
    m

    m
    (let [s (-> m .toUnit str)
          dot-idx (-> s (.indexOf "."))]
      (if (>= dot-idx 0)
        (if (-> s (.substring dot-idx) count (= 2))
          (str s "0")
          s)
        (str s ".00")))))

(comment
  (-> (money "2")  ->str)
  (-> (money "2.1")  ->str)
  (-> (money "2.12")  ->str))


(defn multiply [m factor]
  (when m
    (-> (money m)
        (.multiply factor "HALF_UP"))))

(comment
  (->str (multiply "2.50" 4)))

(defn subtract [m1 m2]
  (cond
    (and m1 m2) (-> (money m1) (.subtract (money m2)))
    m1 m1
    m2 (multiply m2 -1)))

(comment
  (->str (subtract "10" 1))
  (->str (subtract "10" nil))
  (->str (subtract nil 2)))

(defn sum [vals]
  (reduce (fn [ret val]
            (if val
              (-> ret (.add (money val)))
              ret))
          (money 0) vals))

(comment
  (->str (sum [2 "3.20" nil])))

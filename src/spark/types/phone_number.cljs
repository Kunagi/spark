(ns spark.types.phone-number
  (:require
   [clojure.string :as str]
   [spark.rct :refer [test>]]
   [spark.utils :as u]
   [clojure.spec.alpha :as s]))

(def country-prefix--de "49")

(defn ->e123-spaceless [s default-country-prefix-number]
  (when s
    (assert default-country-prefix-number)
    (let [s (str/trim s)

          starts-with+? (str/starts-with? s "+")
          s (if starts-with+?
              (-> s (.substring 1))
              s)

          s (str/replace s #"[^0-9]" "")

          s (if starts-with+?
              (str "+" s)
              s)

          ;; s (str/replace s " " "")
          ;; s (str/replace s "/" "")
          ;; s (str/replace s "-" "")
          ;; s (str/replace s "(" "")
          ;; s (str/replace s ")" "")

          s (if (-> s (.startsWith "00"))
              (str/replace s "00" "+")
              s)
          s (if (-> s (.startsWith "0"))
              (str "+" default-country-prefix-number (-> s (.substring 1)))
              s)]
      s)))

(test> T->e123-spaceless
       (def _tel "+4957519934900")
       (def _de "49")

       (assert (= _tel (->e123-spaceless "+49 5751 9934900" _de)))
       (assert (= _tel (->e123-spaceless "+49 5751-9934900" _de)))
       (assert (= _tel (->e123-spaceless "+49 5751/9934900" _de)))

       (assert (= _tel (->e123-spaceless "05751 9934900" _de)))
       (assert (= _tel (->e123-spaceless "05751/9934900" _de)))
       (assert (= _tel (->e123-spaceless "057519934900" _de)))
       (assert (= _tel (->e123-spaceless "(05751) 9934900" _de)))

       (assert (= _tel (->e123-spaceless "+49.5751+99 und 34900" _de))))

(ns spark.types.phone-number
  (:require
   [clojure.string :as str]
   [spark.rct :refer [test>]]
   [spark.utils :as u]))

(defn ->e123-spaceless [s default-country-prefix-number]
  (when s
    (assert default-country-prefix-number)
    (let [s (str/trim s)
          s (str/replace s " " "")
          s (str/replace s "/" "")
          s (str/replace s "-" "")
          s (str/replace s "(" "")
          s (str/replace s ")" "")
          s (if (-> s (.startsWith "00"))
              (str/replace s "00" "+")
              s)
          s (if (-> s (.startsWith "0"))
              (str "+" default-country-prefix-number (-> s (.substring 1)))
              s)]
      s)))

(test>
 (def _tel "+4957519934900")
 (def _de "49")

 (assert (= _tel (->e123-spaceless "+49 5751 9934900" _de)))
 (assert (= _tel (->e123-spaceless "+49 5751-9934900" _de)))
 (assert (= _tel (->e123-spaceless "+49 5751/9934900" _de)))

 (assert (= _tel (->e123-spaceless "05751 9934900" _de)))
 (assert (= _tel (->e123-spaceless "05751/9934900" _de)))
 (assert (= _tel (->e123-spaceless "057519934900" _de)))
 (assert (= _tel (->e123-spaceless "(05751) 9934900" _de))))

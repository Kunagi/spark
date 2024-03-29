(ns spark.env
  (:require
   [spark.local :as local]
   [spark.utils :as u]))

(defn- format-default [v]
  (when v (str v)))

(defn format-x-on-true
  [b]
  (when b "x"))

(comment
 "true" (format-x-on-true true) := "x"
 "false" (format-x-on-true false) := nil
 )

(def ENV (atom {:formatters {:default format-default
                             :decimal local/format-decimal
                             :eur local/format-eur
                             :date local/format-date
                             :time local/format-time
                             :date+time local/format-date+time
                             :yes-no local/format-yes-no
                             :x-on-true format-x-on-true
                             :millis u/millis->hours-minutes
                             }}))

(defn formatter-from-env [k]
  (let [formatters (-> @ENV :formatters)]
    (or (get formatters k)
        (get formatters :default))))

(defn format
  "Converts the value `v` to a string using the given or the registered formatter."
  [v formatter]
  (let [formatter (if (keyword? formatter)
                    (formatter-from-env formatter)
                    formatter)]
    (tap> formatter)
    (formatter v)))

(comment
  (format (js/Date.) :date)
  (format (js/Date.) :time)
  (format (js/Date.) :date+time)
  (format 20 :eur)
  (format "20.2" :eur)
  (format nil :eur)
  (format 1 :default))

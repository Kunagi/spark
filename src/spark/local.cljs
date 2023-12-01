(ns spark.local
  (:require
   [clojure.string :as str]
   [kunagi.utils.local :as local]
   [spark.money :as money]
   [spark.time :as time]
   [spark.utils :as u]
   [spark.time :as tick]))

(def LANG local/LANG)
(def lang local/lang)
(def set-lang! local/set-lang!)

(def format-decimal local/format-decimal)
(def TEXTS local/TEXTS)
(def textc local/textc)
(def text local/text)
(def format-yes-no local/format-yes-no)

;; * currency

(defn format-currency [lang currency v]
  (when v
    (-> js/Intl
        (.NumberFormat (local/->js-locale-string lang)
                       (clj->js {:style    "currency"
                                 :currency currency}))
        (.format (cond
                   (number? v) v
                   (money/money? v) (money/->number v)
                   :else v)))))

(comment
  (-> js/Intl
      (.NumberFormat "DE-de" (clj->js {:style "currency"
                                       :currency "EUR"}))
      (.format 23.42)))

(defn format-eur
  ([v]
   (format-currency @LANG "EUR" v))
  ([lang v]
   (format-currency lang "EUR" v)))

(comment
 "integer" (format-eur :de 20) := "20,00 €"
 "decimal" (format-eur :de 20.2) := "20,20 €"
 "string" (format-eur :de "20.2") := "20,20 €"
 "money" (format-eur (money/money 200)))

;; * time

(defn ->joda-locale
  ([]
   (->joda-locale @LANG))
  ([lang]
   (cond
     (string? lang) (->joda-locale (keyword lang))
     (= lang :de) (-> js/JSJodaLocale .-Locale .-GERMANY)
     (= lang :en) (-> js/JSJodaLocale .-Locale .-US))))

(defn tick-formatter
  ([pattern]
   (tick-formatter @LANG pattern))
  ([lang pattern]
   (time/formatter pattern (->joda-locale lang))))

(defn formatter--date
  ([]
   (formatter--date @LANG))
  ([lang]
   (tick-formatter
    lang (case
          lang :de "dd.MM.yyyy"
          "yyyy-MM-dd"))))

(defn format-date
  ([v]
   (format-date @LANG v))
  ([lang v]
   (when v
     (if (and (string? v)
              (str/blank? v))
       nil
       (->> (u/->date v)
            (time/format (formatter--date lang)))))))

(comment
 "string" (format-date :de "2020-01-01")

 (->> (u/->date "2020-01-01") (time/format (time/formatter "dd.MM." (->joda-locale :de))))
 (->> (u/->date "2020-01-01") (time/format (time/formatter "E dd.MM." (->joda-locale :de))))
 (->> (u/->date "2020-01-01") (time/format (time/formatter "E dd.MM." (->joda-locale :en))))
 (->> (u/->date "2020-01-01") (time/format (tick-formatter "E dd.MM." )))
 (->> (u/->date (time/instant)) (time/format (tick-formatter "E dd.MM." )))
 (->> (time/instant) (time/format (tick-formatter "E dd.MM." )))

 ;;
 )

(defn format-time
  ([v]
   (format-time @LANG v :minutes))
  ([v truncate-to]
   (format-time @LANG v truncate-to))
  ([lang v truncate-to]
   (when v
     (let [t (cond
               (time/time? v) v
               (string? v) (time/time v)
               :else (-> v u/->instant time/in-berlin time/time))]
       (-> t
           (time/truncate truncate-to)
           str)))))

(comment
 "js/Date" (format-time (js/Date. "2020-01-01T12:21")) := "12:21")

(defn format-date+time
  ([v]
   (format-date+time @LANG v))
  ([lang v]
   (when v
     (let [instant (u/->instant v)
           dt (time/in-berlin instant)]
       (str (format-date lang (time/date dt))
            " "
            (format-time lang (time/time dt) :minutes))))))

(comment
 "js/Date" (format-date+time :de (js/Date.))
 "string" (format-date+time :de "2020-01-01T12:23") := "01.01.2020 12:23")

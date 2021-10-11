(ns spark.local
  (:require
   [clojure.string :as str]
   [tick.alpha.api :as tick]
   [tick.format :as tick.format]

   [spark.rct :refer [tests]]
   [spark.utils :as u]
   [spark.money :as money]
   ))

(defonce LANG (atom :de))

(defn lang []
  @LANG)

;; * currency

(defn ->js-locale-string [lang]
  (cond
    (string? lang) lang
    (= lang :de) "DE-de"
    (= lang :en) "EN-us"))

(defn format-currency [lang currency v]
  (when v
    (-> js/Intl
        (.NumberFormat (->js-locale-string lang)
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
      (.format 23.42))
  )

(defn format-eur
  ([v]
   (format-currency @LANG "EUR" v))
  ([lang v]
   (format-currency lang "EUR" v)))

(tests
 "integer" (format-eur :de 20) := "20,00 €"
 "decimal" (format-eur :de 20.2) := "20,20 €"
 "string" (format-eur :de "20.2") := "20,20 €"
 "money" (format-eur (money/money 200))
 )

;; * time

(defn formatter--date
  ([]
   (formatter--date @LANG))
  ([lang]
   (tick.format/formatter
    (case
        lang :de "dd.MM.yyyy"
        "yyyy-mm-dd"))))

(defn format-date
  ([v]
   (format-date @LANG v))
  ([lang v]
   (when v
     (->> (u/->date v)
          (tick/format (formatter--date lang))))))

(tests
 "string" (format-date :de "2020-01-01"))

(defn format-time
  ([v]
   (format-time @LANG v :minutes))
  ([v truncate-to]
   (format-time @LANG v truncate-to))
  ([lang v truncate-to]
   (when v
     (-> (u/->time v)
         (tick/truncate truncate-to)
         str))))

(tests
 "js/Date" (format-time (js/Date. "2020-01-01T12:21") ) := "12:21")

(defn format-date+time
  ([v]
   (format-date+time @LANG v))
  ([lang v]
   (let [instant (u/->instant v)]
     (str (format-date lang (tick/date instant))
          " "
          (format-time lang (tick/time instant) :minutes)))))

(tests
 "js/Date" (format-date+time :de (js/Date.))
 "string" (format-date+time :de "2020-01-01T12:23") := "01.01.2020 12:23"
 )

;; * texts de

(def texts--de
  {:yes "Ja"
   :no "Nein"
   :ok "OK"
   :cancel "Abbrechen"
   :continue "Weiter"
   :error "Fehler"
   :delete "Löschen"
   :delete-image? "Bild löschen?"
   })

;; * texts

(defonce TEXTS (atom {:de texts--de}))

(defn text
  ([k]
   (text @LANG k nil))
  ([k opts]
   (text @LANG k opts))
  ([lang k opts]
   (when k
     (let [v (get-in @TEXTS [lang k])]
       (cond
         (nil? v)    (name k)
         (fn? v)     (v opts)
         (string? v) v
         :else       (str v)
         ))))
  )

(tests
 (text :yes)
 (text :continue)
 (text nil))

;; * boolean

(defn format-yes-no
  ([b]
   (format-yes-no @LANG b))
  ([lang b]
   (when-not (nil? b)
     (text lang (if b :yes :no) nil))))

(tests
 "nil" (format-yes-no :de nil) := nil
 "true" (format-yes-no :de true) := "Ja"
 "false" (format-yes-no :de false) := "Nein"
 )

(ns spark.time
  (:refer-clojure :exclude [time inc format min max > < >= <=])
  (:require
   #?(:cljs ["@js-joda/locale_de-de" :as js-joda-locale])
   [tick.core :as tick]
   [tick.timezone]))

(println "")
(println "      Setting default locale to GERMANY (spark.time)")
(println "")

#?(:cljs (set! js/JSJodaLocale js-joda-locale)
   :clj (java.util.Locale/setDefault java.util.Locale/GERMANY))

(defn date
  ([]
   (tick/date))
  ([thing]
   (when thing
     (try
       (tick/date thing)
       (catch #?(:cljs :default
                 :clj Exception) ex
         (throw (ex-info (str "Unsupported date string: '" thing "': " ex)
                         {}
                         ex)))))))

(defn time
  ([]
   (tick/time))
  ([thing]
   (when thing
     (tick/time thing))))

(defn date-time
  ([]
   (tick/date-time))
  ([thing]
   (when thing
     (tick/date-time thing))))

(defn instant
  ([]
   (tick/instant))
  ([thing]
   (when thing
     (tick/instant thing))))

(def date? tick/date?)
(def date-time? tick/date-time?)
(def zoned-date-time? tick/zoned-date-time?)
(def time? tick/time?)
(def instant? tick/instant?)
(def inst tick/inst)
(def now tick/now)
(def zoned-date-time tick/zoned-date-time)
(def today tick/today)
(def hour tick/hour)
(def minute tick/minute)
(def month tick/month)
(def year tick/year)
(def hours tick/hours)
(def minutes tick/minutes)
(def seconds tick/seconds)
(def day-of-month tick/day-of-month)
(def new-period tick/new-period)
(def new-duration tick/new-duration)
(def formatter tick/formatter)
(def in tick/in)
(def max tick/max)
(def min tick/min)
(def inc tick/inc)
(def between tick/between)
(def >> tick/>>)
(def << tick/<<)
(def > tick/>)
(def < tick/<)
(def >= tick/>=)
(def <= tick/<=)
(def day-of-week tick/day-of-week)
(def format tick/format)
(def truncate tick/truncate)

(def MONDAY tick/MONDAY)
(def TUESDAY tick/TUESDAY)
(def WEDNESDAY tick/WEDNESDAY)
(def THURSDAY tick/THURSDAY)
(def FRIDAY tick/FRIDAY)
(def SATURDAY tick/SATURDAY)
(def SUNDAY tick/SUNDAY)

#?(:clj (def iso-8601-format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss")))

(defn in-berlin [t]
  (when t
    (-> t (in "Europe/Berlin"))))

(defn instant-in-berlin []
  (-> (instant)
      in-berlin))

(defn millis
  ([]
   #?(:cljs (-> (js/Date.) .getTime)
      :clj (System/currentTimeMillis)))
  ([thing]
   (when thing
     (cond
       (nil? thing) nil
       (number? thing) thing

       #?(:cljs (instance? js/Date thing)
          :clj (instance? java.util.Date thing))
       (-> thing .getTime)

       (tick/date-time? thing) (-> thing tick/inst .getTime)
       (tick/zoned-date-time? thing) (-> thing tick/inst .getTime)

       :else
       #?(:cljs (js/Date.parse thing)
          :clj (-> iso-8601-format
                   (.parse (str thing))
                   .getTime))))))

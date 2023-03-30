(ns spark.time
  (:refer-clojure :exclude [time min max > < >= <=])
  (:require
   [tick.timezone]
   #?(:cljs ["@js-joda/locale_de-de" :as js-joda-locale])
   [tick.core :as tick]))

(prn "\n\n!!! spark.time !!! Setting default locale to GERMANY !!!\n")
#?(:cljs (set! js/JSJodaLocale js-joda-locale)
   :clj (java.util.Locale/setDefault java.util.Locale/GERMANY))

(defn date
  ([]
   (tick/date))
  ([thing]
   (when thing
     (tick/date thing))))

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
(def formatter tick/formatter)
(def max tick/max)
(def min tick/min)
(def >> tick/>>)
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

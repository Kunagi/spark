(ns spark.local-init-de
  (:require
   ["@js-joda/locale_de-de" :as js-joda-locale]
   [spark.local :as local]))

(js/console.log "[spark.local-init-ide]" js-joda-locale)
(set! js/JSJodaLocale js-joda-locale)

(reset! local/LANG :de)

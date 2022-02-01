(ns spark.logging
  (:require
   [spark.utils :as u]))

(defmacro log [event-keyword & {:as event-data}]
  ;; (prn &env)
  (let [event (str event-keyword)
        log-format (u/compiler-option :spark-log-format)
        ;; s (with-out-str (pprint &env))
        ]
    (if event-data
      (case log-format

        :edn `(-> (or js/_spark_logger js/console) (.log ~event (with-out-str (cljs.pprint/pprint ~event-data))))
        :js `(-> (or js/_spark_logger js/console) (.log ~event (cljs.core/clj->js ~event-data)))

        `(if goog.DEBUG
           (-> (or js/_spark_logger js/console) (.log ~event ~event-data))
           (-> (or js/_spark_logger js/console) (.log ~event (cljs.core/clj->js ~event-data)))))

      `(-> (or js/_spark_logger js/console) (.log ~event)))))

(ns spark.logging
  (:require
   [spark.utils :as u])
  )

(defmacro log [event-keyword & {:as event-data}]
  ;; (prn &env)
  (let [event (str event-keyword)
        log-format (u/compiler-option :spark-log-format)
        ;; s (with-out-str (pprint &env))
        ]
    (if event-data
      (case log-format

          :edn `(-> logger (.log ~event (with-out-str (cljs.pprint/pprint ~event-data))))
          :js `(-> logger (.log ~event (cljs.core/clj->js ~event-data)))

          `(if goog.DEBUG
             (-> logger (.log ~event ~event-data))
             (-> logger (.log ~event (cljs.core/clj->js ~event-data)))))

      `(-> logger (.log ~event))))

  #_(let [event (str event-keyword)]
    (try
      (if event-data
        (-> @LOGGER (.log event event-data #_(clj->js event-data)))
        (-> @LOGGER (.log event)))
      (catch :default ex
        (-> @LOGGER (.error "Failed to log" event event-data ex))))))

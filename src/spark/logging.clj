(ns spark.logging
  )

(defmacro log [event-keyword & {:as event-data}]
  (let [event (str event-keyword)]
    (if event-data
      `(if goog.DEBUG
         (-> logger (.log ~event ~event-data))
         (-> logger (.log ~event (clj->js ~event-data))))
      `(-> logger (.log ~event))))

  #_(let [event (str event-keyword)]
    (try
      (if event-data
        (-> @LOGGER (.log event event-data #_(clj->js event-data)))
        (-> @LOGGER (.log event)))
      (catch :default ex
        (-> @LOGGER (.error "Failed to log" event event-data ex))))))

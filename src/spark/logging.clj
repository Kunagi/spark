(ns spark.logging
  )

(defn log [event-keyword & {:as event-data}]
  #_(let [[event-namespace event-name message extra-data]
          (cond

            (instance? js/Error event-keyword)
            ["?" "ERROR" (-> ^js event-keyword .-message) (ex-data event-keyword)]

            (qualified-keyword? event-keyword)
            [(namespace event-keyword)
             (name event-keyword)]

            :else
            (if (keyword? event-keyword)
              ["?" (name event-keyword)]
              ["?" (str event-keyword)]))

          event-data (conform-event-data event-data)
          event-data (if message
                       (assoc event-data :message message)
                       event-data)
          event-data (if extra-data
                       (merge extra-data event-data)
                       event-data)]
      (@WRITER event-namespace event-name event-data))

  `(let [event (str event-keyword)]
    (try
      (if event-data
        (-> @LOGGER (.log event event-data #_(clj->js event-data)))
        (-> @LOGGER (.log event)))
      (catch :default ex
        (-> @LOGGER (.error "Failed to log" event event-data ex))))))

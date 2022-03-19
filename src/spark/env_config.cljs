(ns spark.env-config)

(defonce ENV (atom {}))

(defn set! [k v]
  (js/console.log "[ env-config ]" k "->" v)
  (swap! ENV assoc k v))

(defn get!
  ([k]
   (if-let [v (get @ENV k)]
     v
     (throw (ex-info (str "spark.env-config: Missing value for key `" k "`")
                     {:missing-key k
                      :available-keys (keys @ENV)}))))
  ([k default-value]
   (get @ENV k default-value)))

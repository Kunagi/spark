(ns spark.env-config)

(defonce ENV (atom {}))

(defn set! [k v]
  (js/console.log "[ env-config ]" k "->" v)
  (swap! ENV assoc k v))

(defn get!
  ([k]
   (get @ENV k))
  ([k default-value]
   (get @ENV k default-value)))

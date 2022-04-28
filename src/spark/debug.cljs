(ns spark.debug)

(defonce ACTIVE (atom (boolean goog.DEBUG)))

(defn active? []
  @ACTIVE)

(defn activate []
  (reset! ACTIVE true))


(defonce ITEMS (atom {}))

(defn reg-item [k v]
  (swap! ITEMS (fn [items]
                 (update items k (fn [set]
                                   (conj (or set #{})
                                         v))))))

(defn unreg-item [k v]
  (swap! ITEMS (fn [items]
                 (update items k (fn [set]
                                   (disj set v))))))

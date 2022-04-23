(ns spark.errors
  (:require
   [spark.utils :as u]))

(defn conform [ex]
  (cond

    (nil? ex)
    {}

    (string? ex)
    {:message ex}

    #?(:cljs (instance? js/Error ex))
    #?(:cljs {:message (-> ^js ex .-message)
              :data (ex-data ex)
              :stacktrace (-> ^js ex .-stack)
              :cause (when-let [c (ex-cause ex)]
                       (conform c))})

    (map? ex)
    (if-let [message (-> ex :message)]
      {:message message
       :data (-> ex :data)
       :stacktrace (-> ex :stacktrace)
       :cause (when-let [cause (-> ex :cause)]
                (conform cause))}
      {:message nil
       :data ex})

    :else
    {:data (u/->edn ex)}))

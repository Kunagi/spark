(ns spark.gcf.cmd
  (:require
   [tick.locale-en-us]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.firestore :as firestore]
   [spark.gcf :as gcf]))

(defn execute-command> [commands-map command-key args]
  (log ::execute-command>
       :command command-key
       :args args)
  (let [command (get commands-map command-key)
        _ (u/assert command (str "Command does not exist: " command-key))
        f> (get command :f>)
        _ (u/assert (fn? f>))
        uid (-> args :uid)
        _ (u/assert (or uid (-> command :public)) "Permission denied")]
    (f> args command)))

(defn handle-cmd-request> [commands-map ^js req]
  (let [params (->> req
                    .-query
                    js->clj
                    (reduce (fn [m [k v]]
                              (assoc m (keyword k) v))
                            {}))
        uid (when-let [auth (->> req .-auth)]
              (-> ^js auth .-uid))]
    (log ::hande-cmd-request>
         :request params
         :uid uid)
    (let [command-key (-> params :cmd keyword)
          command-args (-> params
                           (dissoc :cmd)
                           (assoc :uid uid))]
      (execute-command> commands-map command-key command-args)))
  )

(def default-commands-map
  {:dummy {:public true
           :f> (fn [args command]
                 {:args args
                  :command command})}})

(defn exports [commands-map]
  (let [commands-map (->> commands-map
                          (merge default-commands-map)
                          (reduce (fn [m [k v]]
                                    (assoc m k (assoc v :key k)))
                                  {}))]
    {

     :cmdRequest
     (gcf/on-request--format-output> (partial handle-cmd-request> commands-map))

     }))
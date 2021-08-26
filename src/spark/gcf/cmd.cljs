(ns spark.gcf.cmd
  (:require
   [tick.locale-en-us]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.money :as money]
   [spark.firestore :as firestore]
   [spark.db :as db]
   [spark.gcf :as gcf]))

(defn assert-arg [arg-key arg-def arg-value]
  (u/assert arg-value (str "Missing arg: " arg-key))

  (case (-> arg-def :type)
    :money (u/assert (money/money? arg-value)
                     (str "type :money expected: " arg-value) )
    :eur (u/assert (money/money? arg-value)
                   (str "type :eur expected: " arg-value) )
    nil)
  )

(defn assert-args [command args]
  (doseq [[arg-key arg-def] (-> command :args)]
    (assert-arg arg-key arg-def (get args arg-key))))

(defn load-args> [args command]
  (u/=> (u/all> (map (fn [[k v]]
                       (if-let [doc-col-name (get-in command [:args k :get-doc])]
                         (let [path (str doc-col-name "/" v)]
                           (u/=> (db/get> path)
                                 (fn [doc]
                                   (u/assert doc (str "Missing doc: " path))
                                   [k doc])))
                         [k v]))
                     args))
        (fn [args-as-kvs]
          (reduce (fn [m [k v]]
                    (assoc m k v))
                  {} args-as-kvs))))

(defn execute-command> [commands-map command-key args]
  (log ::execute-command>
       :command command-key
       :args args)
  (let [command (get commands-map command-key)
        _ (u/assert command (str "Command does not exist: " command-key))
        f> (get command :f>)
        _ (u/assert (fn? f>))
        uid (-> args :uid)
        _ (u/assert (or uid (-> command :public)) "Permission denied")
        _ (assert-args command args)]
    (u/=> (load-args> args command)
          (fn [args]
            (f> args command)))))

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

(defn handle-cmd-call> [commands-map data ^js context]
  (let [uid (when-let [auth (-> context .-auth)]
              (-> ^js auth .-uid))]
    (log ::hande-cmd-call>
         :data data
         :uid uid)
    (let [command-key (-> data :cmd keyword)
          command-args (-> data
                           (dissoc :cmd)
                           (assoc :uid uid))]
      (execute-command> commands-map command-key command-args))))

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

     :cmdCall
     (gcf/on-call (partial handle-cmd-call> commands-map))

     }))

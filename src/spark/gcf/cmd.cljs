(ns spark.gcf.cmd
  (:require-macros [spark.gcf.cmd :refer [def-cmd]])
  (:require
   ["firebase-functions" :as firebase-functions]
   [kunagi.utils :as ku]
   [spark.db :as db]
   [spark.gcf :as gcf]
   [spark.logging :refer [log]]
   [spark.money :as money]
   [spark.utils :as u]))

(defn assert-arg [arg-key arg-def arg-value]
  (when-not (-> arg-def :optional)
    (u/assert arg-value (str "Missing arg: " arg-key)))

  (case (-> arg-def :type)
    :money (u/assert (money/money? arg-value)
                     (str "type :money expected: " arg-value))
    :eur (u/assert (money/money? arg-value)
                   (str "type :eur expected: " arg-value))
    nil))

(defn assert-args [command args]
  (doseq [[arg-key arg-def] (-> command :args)]
    (assert-arg arg-key arg-def (get args arg-key))))

(defn converge-arg-value-to-type [v t]
  (case t
    :keyword (keyword v)
    :string (str v)
    :int (js/parseInt v)
    v))

(defn load-args> [args command]
  (u/=> (u/all> (map (fn [[k v]]
                       (if-not v
                         [k v]
                         (let [arg-def (get-in command [:args k])
                               optional? (-> arg-def :optional boolean)]

                           (cond

                             (-> arg-def :type)
                             [k (converge-arg-value-to-type v (-> arg-def :type))]

                             (-> arg-def :get-doc)
                             (let [_ (u/assert (string? v))
                                   path (str (-> arg-def :get-doc) "/" v)]
                               (u/=> (db/get> path)
                                     (fn [doc]
                                       (when-not optional?
                                         (u/assert doc (str "Missing doc: " path)))
                                       [k doc])))

                             :else
                             [k v]))))

                     args))
        (fn [args-as-kvs]
          (reduce (fn [m [k v]]
                    (assoc m k v))
                  {} args-as-kvs))))

(defn execute-command> [commands-map command-key args]
  (log ::execute-command>
       :value command-key
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
    (log ::handle-cmd-request>
         :request params
         :uid uid)
    (let [command-key (-> params :cmd keyword)
          command-args (-> params
                           (dissoc :cmd)
                           (assoc :uid uid))]
      (execute-command> commands-map command-key command-args))))

(defn handle-cmd-call> [commands-map data ^js context]
  (let [uid (when-let [auth (-> context .-auth)]
              (-> ^js auth .-uid))]
    (log ::handle-cmd-call>
         :data data
         :uid uid)
    (let [command-key (-> data :cmd keyword)
          command-args (-> data
                           (dissoc :cmd)
                           (assoc :uid uid))]
      (-> (execute-command> commands-map command-key command-args)
          (.catch (fn [error]
                    #_(log ::handle-cmd-call>--error
                           :command command-key
                           :args command-args
                           :error error)
                    #_(-> firebase-functions
                          .-logger
                          (.error (str "Error executing command via cmdCall GCF: "
                                       (name (or command-key
                                                 :_no-command))
                                       ":")
                                  error))
                    #_(js/console.error (str "Error executing command: "
                                             (name (or command-key
                                                       :_no-command))
                                             ":")
                                        error)
                    #_(u/resolve> {:_spark-cmd-error (ku/error->data error)})
                    #_(throw (js/Error. (str "Error executing command: " (name (or command-key
                                                                                   :_no-command))
                                             " | " error)
                                        (clj->js {:cause error})))
                    (throw (ex-info (str "Error executing command '"
                                         (name (or command-key :_no-command))
                                         "': " error)
                                    {:command command-key
                                     :args command-args
                                     ;; :error error
                                     }
                                    error))))))))

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
    {:cmdRequest
     (gcf/on-request--format-output> (partial handle-cmd-request> commands-map))

     :cmdCall
     (gcf/on-call (partial handle-cmd-call> commands-map)
                  {:timeoutSeconds 60
                   :memory "8GB"})}))

;; * registry

(defonce CMDS (atom {}))

(defn reg-cmd [cmd]
  (u/assert (-> cmd :id) "Invalid cmd" cmd)
  (u/assert (-> cmd :public boolean?) "Invalid cmd" cmd)
  (u/assert (-> cmd :f>) "Invalid cmd" cmd)
  (let [id (-> cmd :id)]
    ;; (log ::reg-cmd
    ;;      :id id)
    (swap! CMDS assoc id cmd)
    cmd))

(defn registered-cmds []
  @CMDS)

;; * Build-in commands

(def-cmd version-info
  {:public true
   :args {}
   :f> (fn [{:keys []}]
         {:version (gcf/current-version)
          :version-time (gcf/current-version-time)})})

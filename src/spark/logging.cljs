(ns spark.logging
  (:require
   [cljs.pprint :refer [pprint]]))


(defn format-clojure-value [data]
  (when data
    (try
      (with-out-str (pprint data))
      (catch :default _ex
        nil))))


(defn format-event-data [data]
  (when data
    (if (and ^boolean js/goog.DEBUG
             (exists? js/window)
             )
      data
      ;; (clj->js data)
      (with-out-str (pprint data))
      ;; (str data)
      )))


(defn console-writer [^js console event-namespace event-name event-data]
  (try
    (let [event-s (str "%cLOG" " %c" event-namespace " %c" event-name)
          message (get event-data :message)
          event-data (dissoc event-data :message)
          event-s (if message
                    (str event-s "\n" message)
                    event-s)
          col-1 "background-color: blue; color: white; padding: 3px;"
          col-2 "background-color: #5472d3; color: white; padding: 3px;"
          col-3 "background-color: #002171; color: white; padding: 3px;"
          data-s (when-not (empty? event-data) (format-event-data event-data))]
      (if data-s
        (.log console event-s col-1 col-2 col-3 "\n" data-s)
        (.log console event-s col-1 col-2 col-3)))
    (catch :default _ex
      (console-writer console event-namespace event-name (format-clojure-value event-data)))))

(defonce WRITER (atom (partial console-writer js/console)))

(defn- conform-event-data [event-data]
  (if (and (-> event-data count (= 1))
           (-> event-data first first keyword? not)
           (-> event-data first second nil?))
    (let [value (-> event-data first first)]
      (cond
        (map? value) value
        (instance? js/Error value) (-> (ex-data value)
                                       (assoc :message (-> ^js value .-message)))
        :else {:_ value}))
    event-data))

(comment
  (conform-event-data (ex-info "error message" {:a "b"}))
  (conform-event-data {:a "a" :b "b"})
  (conform-event-data {:a "a"})
  (conform-event-data {:a nil}))

(defn log [event-keyword & {:as event-data}]
  (let [[event-namespace event-name message extra-data]
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
                     event-data)
        ]
    (@WRITER event-namespace event-name event-data)))

(comment
  (type (ex-info "hallo" {:x 1}))
  (instance? js/Error (ex-info "hallo" {}))
  (ex-data (ex-info "hey" {:p1 "h"}))
  (log ::dummy-with-data
       :param-1 "witek"
       :context {:this 1
                 :and "and that" :more [1 2 3 4 5] :asdlfjkasldfkj "asöldj aslödkj asöldkfj asöldfj "})
  (log (ex-info "Error here" {:with :data}))
  (log ::exception (ex-info "Error Here" {:with "data"}))
  (log ::with-message
       :message "This is the message.\nMultiline!")
  (log :simple)
  (log ::auth-state-changed
       :user nil)
  (log ::test
       "missing key"))

(defonce TAP (atom nil))

(defn install-tap []
  (log ::install-tap)
  (swap! TAP (fn [old-tap]
               (when old-tap
                 (remove-tap old-tap))
               (let  [tap (fn [value]
                            (log ::tap> :_ value))]
                 (add-tap tap)
                 tap))))

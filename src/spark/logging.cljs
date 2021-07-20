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
    (.log
     console
     (str "%c" event-namespace " %c" event-name)
     "background-color: #5472d3; color: white; padding: 3px;"
     "background-color: #002171; color: white; padding: 3px;"
     "\n"
     (format-event-data event-data))
    (catch :default _ex
      (console-writer console event-namespace event-name (format-clojure-value event-data)))))

(defonce WRITER (atom (partial console-writer js/console)))

(defn- conform-event-data [event-data]
  (if (and (-> event-data count (= 1))
           (-> event-data first first keyword? not)
           (-> event-data first second nil?))
    (let [value (-> event-data first first)]
      (if (map? value)
        value
        {:_ value}))
    event-data))

(comment
  (conform-event-data {:a "a" :b "b"})
  (conform-event-data {:a "a"})
  (conform-event-data {:a nil}))

(defn log [event-keyword & {:as event-data}]
  (let [[event-namespace event-name] (if (qualified-keyword? event-keyword)
                                       [(namespace event-keyword)
                                        (name event-keyword)]
                                       (if (keyword? event-keyword)
                                         ["?" (name event-keyword)]
                                         ["?" (str event-keyword)]))
        event-data                   (conform-event-data event-data)
        ]
    (@WRITER event-namespace event-name event-data)))

(comment
  (log ::dummy-with-data
       :param-1 "witek"
       :context {:this 1
                 :and "and that" :more [1 2 3 4 5] :asdlfjkasldfkj "asöldj aslödkj asöldkfj asöldfj "})
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

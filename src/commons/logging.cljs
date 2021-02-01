(ns commons.logging
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
             (exists? js/window))
      data
      (clj->js data))))


(defn console-writer [^js console event-namespace event-name event-data]
  (try
    (.log
     console
     (str "%c" event-namespace " %c" event-name)
     "background-color: #5472d3; color: white; padding: 3px;"
     "background-color: #002171; color: white; padding: 3px;"
     (format-event-data event-data))
    (catch :default _ex
      (console-writer console event-namespace event-name (format-clojure-value event-data)))))


(defonce WRITER (atom (partial console-writer js/console)))


(defn log [event-keyword & {:as event-data}]
  (let [[event-namespace event-name] (if (qualified-keyword? event-keyword)
                                       [(namespace event-keyword)
                                        (name event-keyword)]
                                       (if (keyword? event-keyword)
                                         ["?" (name event-keyword)]
                                         ["?" (str event-keyword)]))]
    (@WRITER event-namespace event-name event-data)))

(ns spark.logging
  #?(:cljs (:require-macros [spark.logging :refer [log]]))
  (:require
  [kunagi.utils.logging :as kunagi-logging]))


#?(:clj
   (defmacro log [event-keyword & data-kvs]
     `(kunagi-logging/log ~event-keyword ~@data-kvs)))

#?(:clj
   (defmacro log-error [event-keyword & data-kvs]
     `(kunagi-logging/log-error ~event-keyword ~@data-kvs)
     ))

(ns spark.logging
  (:require-macros [spark.logging :refer [log]])
  (:require
   [cljs.pprint :refer [pprint]]
   [spark.env-config :as env-config]))

;; (def logger js/console)

(set! js/_spark_logger js/console)

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

(when goog.DEBUG
  (install-tap))

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

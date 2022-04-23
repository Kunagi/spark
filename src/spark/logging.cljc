(ns spark.logging
  #?(:cljs (:require-macros [spark.logging :refer [log]]))
  (:require
   #?(:cljs [cljs.pprint :refer [pprint]]
      :clj [clojure.pprint :refer [pprint]])))

;; * log macro

#?(:clj
   (defn compiler-option [k]
     (when cljs.env/*compiler*
       (get-in @cljs.env/*compiler* [:options k]))))

#?(:clj
   (defn ->log-expr [event event-data]
     (case (compiler-option :spark-log-format)

       :edn `(-> (or js/_spark_logger js/console) (.log ~event (with-out-str (cljs.pprint/pprint ~event-data))))
       :js `(-> (or js/_spark_logger js/console) (.log ~event (cljs.core/clj->js ~event-data)))

       `(if goog.DEBUG
          (-> (or js/_spark_logger js/console) (.log ~event ~event-data))
          (-> (or js/_spark_logger js/console) (.log ~event (cljs.core/clj->js ~event-data)))))))

#?(:clj
   (defmacro log [event-keyword & {:as event-data}]
     (->log-expr (str event-keyword) event-data)))

;; * logging

#?(:cljs
   (set! js/_spark_logger js/console))

;; * tap

(defonce TAP (atom nil))

#?(:cljs
   (defn install-tap []
     (log ::install-tap)
     (swap! TAP (fn [old-tap]
                  (when old-tap
                    (remove-tap old-tap))
                  (let  [tap (fn [value]
                               (log ::tap> :_ value))]
                    (add-tap tap)
                    tap)))))

#?(:cljs
   (when goog.DEBUG
     (install-tap)))

;; * ---

(comment
  (log ::with-message
       :message "This is the message.\nMultiline!")
  (log :simple)
  (type (ex-info "hallo" {:x 1}))
  (instance? js/Error (ex-info "hallo" {}))
  (ex-data (ex-info "hey" {:p1 "h"}))
  (log ::dummy-with-data
       :param-1 "witek"
       :context {:this 1
                 :and "and that" :more [1 2 3 4 5] :asdlfjkasldfkj "asöldj aslödkj asöldkfj asöldfj "})
  (log (ex-info "Error here" {:with :data}))
  (log ::exception (ex-info "Error Here" {:with "data"}))
  (log ::auth-state-changed
       :user nil)
  (log ::test
       "missing key"))

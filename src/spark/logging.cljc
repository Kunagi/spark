(ns spark.logging
  #?(:cljs (:require-macros [spark.logging :refer [log]]))
  (:require
   #?(:gcf ["firebase-functions" :as firebase-functions])
   #?(:cljs [cljs.pprint :as pprint]
      :clj [clojure.pprint :as pprint])
   [clojure.string :as str]))

;; * logging

#?(:gcf
   (defn log-with-gcf [event event-data]
     (.write ^js (.-logger firebase-functions)
             (clj->js
              {:severity "DEBUG"
               :message (str (namespace event)
                             " | "
                             (name event))
               :ns (namespace event)
               :event (name event)
               :data event-data}))))

(defn log-with-println [event event-data]
  (println event (when event-data (with-out-str (pprint/pprint event-data)))))

;; * log macro

#?(:clj
   (defn compiler-option [k]
     (when cljs.env/*compiler*
       (get-in @cljs.env/*compiler* [:options k]))))

(def col--spark "#8d6e63")
(def col--app "#6d4c41")
(def col--event "#0277bd")

(def css--ns--spark (str "background-color: " col--spark "; color: white; padding: 2px 4px; border-radius: 4px;"))
(def css--ns--app (str "background-color: " col--app "; color: white; padding: 2px 4px; border-radius: 4px;"))
(def css--event (str "background-color: " col--event "; color: white; font-weight: bold; padding: 2px 4px; border-radius: 4px; margin-left: 4px;"))

#?(:clj
   (defn ->log-expr [event event-data]

     (case (compiler-option :spark-logging-mode)

       :gcf
       ;; `(let [logger (-> firebase-funcotins .-logger)]
       ;;    (.log logger "[gcf]" ~event ~event-data))
       ;; `(.log js/console "[gcf]" ~event ~event-data)
       ;; `(.log (.-logger firebase-functions) "[gcf]" ~event ~event-data)
       `(log-with-gcf ~event ~event-data)

       :browser-console
       (let [event-expr (str "%c" (namespace event)
                             "%c" (name event))]
         (if (if (-> event namespace (str/starts-with? "spark."))
               css--ns--spark css--ns--app)
           `(.log js/console ~event-expr css--ns--spark css--event ~@[event-data])
           `(.log js/console ~event-expr css--ns--app css--event ~@[event-data])))

       ; else
       `(log-with-println ~event ~event-data))))

#?(:clj
   (defmacro log [event-keyword & {:as event-data}]
     (->log-expr event-keyword event-data)))

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

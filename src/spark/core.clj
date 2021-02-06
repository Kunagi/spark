(ns spark.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))


;; (defmacro expect [what provided-value]
;;   nil)


;; TODO disable for production
(defmacro def-test [[sym & refers] & examples]
  (when (= :dev (:shadow.build/mode &env))
    (let [symbol-name (-> sym name )
          calling-namespace-name (name (ns-name *ns*))
          examples examples
          devcard {:id (str calling-namespace-name "/" symbol-name)
                   :namespace calling-namespace-name
                   :symbol symbol-name}
          examples (mapv (fn [example]
                           {:code (with-out-str (pprint example))
                            :f `(fn [~'expect]
                                  ~example)})
                         examples)]
      `(reg-test
        (assoc ~devcard
               :type :fn
               :examples ~examples)))))


(defn- complete-opts [opts sym schema-name]
  (let [symbol-name (-> sym name)
        calling-namespace-name (name (ns-name *ns*))
        id (str calling-namespace-name "/" symbol-name)]
    (assoc opts
           (keyword schema-name "id") id
           (keyword schema-name "namespace") calling-namespace-name)))


(defmacro def-doc [sym [opts & fields]]
  (let [opts (complete-opts opts sym "doc-schema")]
    `(def ~sym
       [:map
        ~opts
        ~@fields])))


(defmacro def-subdoc [sym [opts & fields]]
  (let [opts (complete-opts opts sym "subdoc-schema")]
    `(def ~sym
       [:map
        ~opts
        ~@fields])))

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
           (keyword schema-name "symbol") symbol-name
           (keyword schema-name "namespace") calling-namespace-name)))


(defmacro def-field [sym [type opts & fields]]
  (let [opts (complete-opts opts sym "field-schema")
        opts (assoc opts :id (-> sym name str/lower-case keyword))]
    `(def ~sym
       [~type
        ~opts
        ~@fields])))


(defmacro def-doc  [sym [opts & fields]]
  (let [opts (complete-opts opts sym "doc-schema")]
    `(def ~sym
       (init-doc-schema
        [:map
         ~opts
         ~@fields]))))



(defmacro def-subdoc [sym [opts & fields]]
  (let [opts (complete-opts opts sym "subdoc-schema")]
    `(def ~sym
       [:map
        ~opts
        ~@fields])))


(defmacro def-cmd  [sym opts]
  (let [opts (complete-opts opts sym "cmd")]
    `(def ~sym ~opts)))


(defmacro def-query  [sym opts]
  (let [opts (complete-opts opts sym "query")]
    `(def ~sym ~opts)))


(defmacro def-page  [sym opts]
  (let [opts (complete-opts opts sym "page")]
    `(def ~sym ~opts)))


(defmacro def-spa  [sym opts]
  (let [opts (complete-opts opts sym "spa")]
    `(def ~sym ~opts)))

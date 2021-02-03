(ns spark.models
  (:require
   [clojure.string :as str]))


(defmacro def-model [sym model-def]
  (when-not (vector? model-def)
    (throw (ex-info (str "Invalid call to def-model")
                    {:sym sym
                     :model-def model-def})))
  (let [[constructor model] model-def
        symbol-name (-> sym name )
        calling-namespace-name (name (ns-name *ns*))
        model (assoc model
                     :model/id (str calling-namespace-name "/" symbol-name)
                     :model/namespace calling-namespace-name
                     :model/symbol symbol-name
                     :model/constructor constructor)]
    `(def ~sym (-> ~model ~constructor reg-model))))

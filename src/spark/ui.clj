(ns spark.ui
  (:require
   [clojure.pprint :refer [pprint]]
   [spark.react :as r]))


(defmacro defnc [& body ] `(r/defnc ~@body))
(defmacro $ [type & args] `(r/$ ~type ~@args))
(defmacro <> [& children] `(r/<> ~@children))
(defmacro use-state [& body] `(r/use-state ~@body))
(defmacro use-effect [& body] `(r/use-effect ~@body))



(defmacro devcard [sym & examples]
  (let [symbol-name (-> sym name )
        calling-namespace-name (name (ns-name *ns*))
        examples examples
        devcard {:id (str calling-namespace-name "/" symbol-name)
                 :namespace calling-namespace-name
                 :symbol symbol-name}
        examples (mapv (fn [example]
                         {:code (with-out-str (pprint example))
                          :f `(fn []
                                ~example)})
                       examples)]
    `(reg-devcard
      (assoc ~devcard
             :examples ~examples))))

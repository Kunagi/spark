(ns spark.gcf.cmd)

(defmacro def-cmd [sym cmd]
  {:clj-kondo/ignore [:clojure-lsp/unused-public-var :unused-public-var]}
  (let [cmd (assoc cmd :id (keyword (str sym)))]
    `(def ~sym (reg-cmd ~cmd))))

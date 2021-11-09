(ns spark.gcf.cmd)

(defmacro def-cmd [sym cmd]
  (let [cmd (assoc cmd :id (keyword (str sym)))]
    `(def ~sym (reg-cmd ~cmd))))

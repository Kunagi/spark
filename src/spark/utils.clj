(ns spark.utils
  (:require
   [clojure.pprint :refer [pprint]]
   ))

(defmacro assert [assertion & [message data]]
  (let [assertion-formated (with-out-str (pprint assertion))
        in-function (str (-> &env :fn-scope first :info :ns) "." (-> &env :fn-scope first :name))
        in-file (-> &env :ns :meta :file)
        in-line (-> &env :line)
        ]
    `(let [result# ~assertion]
       (when-not result#
         (throw (ex-info (str "Assertion failed.\n"
                              ~message
                              ~assertion-formated "\n"
                              "in " ~in-function "(" ~in-file ":" ~in-line ")")
                         (assoc ~data
                                :value result#
                                :form '~assertion
                                :in-function ~in-function
                                :in-line ~in-line
                                :in-file ~in-file
                                ))))
       result#)))

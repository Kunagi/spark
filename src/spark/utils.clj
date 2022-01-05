(ns spark.utils
  (:refer-clojure :exclude [assert])
  (:require
   [clojure.pprint :refer [pprint]]))

(defn compiler-option [k]
  (when cljs.env/*compiler*
    (get-in @cljs.env/*compiler* [:options k])))

(defmacro assert [assertion & [message data]]
  (let [assertion-formated (with-out-str (pprint assertion))
        in-function (str (-> &env :fn-scope first :info :ns) "." (-> &env :fn-scope first :name))
        in-file (-> &env :ns :meta :file)
        in-line (-> &env :line)]
    `(let [result# ~assertion]
       (when-not result#
         (spark.logging/log ::assert--failed
                            :message ~message
                            :data ~data)
         (throw (ex-info (str
                          (when ~message (str  ~message "\n"))
                          "Assertion failed.\n"
                          ~assertion-formated
                          "in " ~in-function " (" ~in-file ":" ~in-line ")")
                         (assoc ~data
                                :value result#
                                :assertion-form '~assertion
                                :in-function ~in-function
                                :in-line ~in-line
                                :in-file ~in-file))))
       result#)))

(defmacro get-env-example []
  (let [s (str &env)]
    `~s))

(defmacro get-compiler-option-example []
  (let [s (with-out-str (pprint (get-in @cljs.env/*compiler* [:options])))]
    `~s))

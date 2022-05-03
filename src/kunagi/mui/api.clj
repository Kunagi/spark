(ns kunagi.mui.api
  (:require
   [kunagi.mui.core :as core]))

(defmacro $ [type & args] `(core/$ ~type ~@args))
(defmacro <> [& children] `(core/<> ~@children))
(defmacro create-context [& body] `(core/create-context ~@body))
(defmacro provider [opts & children] `(core/provider ~opts ~@children))
(defmacro use-context [& body] `(core/use-context ~@body))
(defmacro use-state [& body] `(core/use-state ~@body))
(defmacro use-effect [& body] `(core/use-effect ~@body))
(defmacro use-memo [& body] `(core/use-memo ~@body))

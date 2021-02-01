(ns commons.context
  (:require
   [helix.core :as helix]
   [helix.hooks :as hooks]
   [helix.dom :as d]))

(defmacro provider [config & children] `(helix/provider ~config ~@children))

(defmacro use-effect [& args] `(hooks/use-effect ~@args))

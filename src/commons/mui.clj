(ns commons.mui
  (:require
   [helix.core :as helix]
   [helix.hooks :as hooks]
   [helix.dom :as d]))



;; https://cljdoc.org/d/lilactown/helix/0.0.13/doc/pro-tips
(defmacro defnc [type params & body]
  (let [[docstring params body] (if (string? params)
                                  [params (first body) (rest body)]
                                  [nil params body])
        opts? (map? (first body)) ;; whether an opts map was passed in
        opts (if opts?
               (first body)
               {})
        body (if opts?
               (rest body)
               body)
        ;; feature flags to enable by default
        default-opts {:helix/features {:fast-refresh true
                                       :check-invalid-hooks-usage true}}]
    `(helix.core/defnc ~type ~@(when docstring [docstring]) ~params
       ;; we use `merge` here to allow indidivual consumers to override feature
       ;; flags in special cases
       ~(merge default-opts opts)
       ~@body)))


(defmacro $ [type & args] `(helix/$ ~type ~@args))
(defmacro <> [& children] `(helix/<> ~@children))

(defmacro use-state [& body] `(hooks/use-state ~@body))
(defmacro use-effect [& body] `(hooks/use-effect ~@body))

(defmacro div [& body] `(d/div ~@body))
(defmacro span [& body] `(d/span ~@body))
(defmacro img [& body] `(d/img ~@body))

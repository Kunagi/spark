(ns spark.react
  (:require
   [helix.core :as helix]
   [helix.hooks :as helix-hooks]))


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
(defmacro create-context [& body] `(helix/create-context ~@body))
(defmacro use-context [& body] `(helix/use-context ~@body))
(defmacro provider [config & children] `(helix/provider ~config ~@children))

(defmacro use-state [& body] `(helix-hooks/use-state ~@body))
(defmacro use-effect [& body] `(helix-hooks/use-effect ~@body))

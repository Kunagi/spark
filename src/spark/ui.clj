(ns spark.ui
  (:require
   [clojure.string :as str]

   [kunagi.utils.logging :as logging]
   [kunagi.mui.api :as kui]
   [kunagi.mui.core :as kui.core]

   [spark.core :as spark]))

(defmacro defnc [& body] `(kunagi.mui.core/defnc ~@body))
(defmacro $ [& body] `(kunagi.mui.api/$ ~@body))
(defmacro <> [& children] `(kunagi.mui.api/<> ~@children))
(defmacro create-context [& body] `(kunagi.mui.api/create-context ~@body))
(defmacro use-state [& body] `(kunagi.mui.api/use-state ~@body))
(defmacro use-context [& body] `(kunagi.mui.api/use-context ~@body))
(defmacro use-effect [& body] `(kunagi.mui.api/use-effect ~@body))
(defmacro use-ref [& body] `(kunagi.mui.api/use-ref ~@body))
(defmacro provider [& body] `(kunagi.mui.api/provider ~@body))

(defmacro div [& body] `(kunagi.mui.api/div ~@body))
(defmacro span [& body] `(kunagi.mui.api/span ~@body))

(defmacro def-page [& body] `(kunagi.mui.pages/def-page ~@body))

(defmacro def-ui [type params & body]
  (let [[docstring params body] (if (string? params)
                                  [params (first body) (rest body)]
                                  [nil params body])

        params (if (-> params first map?)
                 params
                 [{:keys params}])

        opts? (map? (first body))
        opts  (if opts?
                (first body)
                {})
        body  (if opts?
                (rest body)
                body)

        lets []

        lets (if-let [syms (get opts :from-context)]
               (let [lets (into lets [`~'context_ `(use-spark-context)])]
                 (reduce (fn [lets sym]
                           (into lets
                                 [`~sym `(or ~sym
                                             (get ~'context_ ~(keyword sym)))]))
                         lets syms))
               lets)

        opts (if-let [wrap-memo-props (get opts :wrap-memo-props)]
               (let [a (symbol "a")
                     b (symbol "b")
                     wrap-memo-props (mapv #(if (symbol? %)
                                              (keyword (name %))
                                              %)
                                           wrap-memo-props)]
                 (update opts :wrap conj `(kui/memo
                                           (fn [~a ~b]
                                             (= (select-keys ~a ~wrap-memo-props)
                                                (select-keys ~b ~wrap-memo-props))))))
               opts)

        body (if (seq lets)
               `((let [~@lets]
                   ~@body))
               body)]
    `(defnc
       ~type
       ~@(when docstring [docstring])
       ~params
       ~opts
       ~@body)))

(defmacro try> [expr error-event calling-context]
  (let [error-event (or error-event
                        :spark.ui.unknown-ns/error)]
    `(ku/try>
      ~expr
      #(logging/log-error ~error-event
                          :calling-context ~calling-context
                          :error (ku/error->data %1)
                          :error-context %2))))

(defmacro def-ui-showcase [id-keyword component]
  (when (= :dev (:shadow.build/mode &env))
    `(reg-showcase ~id-keyword
                   {:component (fn [] ~component)
                    :code '~component})))

(def html-element kui.core/html-element)

(defmacro center [& style-and-children]
  (html-element :span style-and-children "center" nil))

(defmacro right [& style-and-children]
  (html-element :div style-and-children "right" nil))

(defmacro center-div [& style-and-children]
  (html-element :div style-and-children "center" nil))

(defmacro center-text [& style-and-children]
  (html-element :div style-and-children "center-text" nil))

(defmacro pre-wrap [& style-and-children]
  (html-element :div style-and-children nil {:white-space :pre-wrap}))

(defmacro icon [icon-name & style-and-children]
  (let [icon-name (if (keyword? icon-name)
                    (name icon-name)
                    icon-name)]
    (html-element :div
                  (conj style-and-children icon-name)
                  "material-icons" nil)))

(defmacro imgdiv [img-url & style-and-children]
  (html-element :div style-and-children nil
                {:background-image    `(str "url(" ~img-url ")")
                 :background-repeat   "no-repeat"
                 :background-position "center"
                 :background-size     "cover"}))

;;; grid

(defn- grid-div [grid-template-columns gap style-and-children]
  (let [grid-template-columns (if-not (vector? grid-template-columns)
                                grid-template-columns
                                (mapv #(if (number? %)
                                         (str % "px")
                                         %)
                                      grid-template-columns))]
    (html-element :div style-and-children
                  (str "grid-" gap) {:grid-template-columns `~grid-template-columns})))

(defmacro grid [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 1 style-and-children))
(defmacro grid-0 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 0 style-and-children))
(defmacro grid-1 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 1 style-and-children))
(defmacro grid-2 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 2 style-and-children))
(defmacro grid-3 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 3 style-and-children))
(defmacro grid-4 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 4 style-and-children))
(defmacro grid-5 [grid-template-columns & style-and-children]
  (grid-div grid-template-columns 5 style-and-children))

(defn- div-class [class style-and-children]
  (html-element :div style-and-children
                class nil))

(defmacro stack [& children] (div-class "stack" children))
(defmacro stack-0 [& children] (div-class "stack-0" children))
(defmacro stack-1 [& children] (div-class "stack-1" children))
(defmacro stack-2 [& children] (div-class "stack-2" children))
(defmacro stack-3 [& children] (div-class "stack-3" children))
(defmacro stack-4 [& children] (div-class "stack-4" children))
(defmacro stack-5 [& children] (div-class "stack-5" children))

(defmacro flex [& children] (div-class "flex" children))
(defmacro flex-0 [& children] (div-class "flex-0" children))
(defmacro flex-1 [& children] (div-class "flex-1" children))
(defmacro flex-2 [& children] (div-class "flex-2" children))
(defmacro flex-3 [& children] (div-class "flex-3" children))
(defmacro flex-4 [& children] (div-class "flex-4" children))
(defmacro flex-5 [& children] (div-class "flex-5" children))

(defmacro map$ [component item-key items]
  `(for [item# ~items]
     (kui/$ ~component
            {:key      item#
             ~item-key item#})))

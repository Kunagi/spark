(ns spark.ui
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]

   [spark.react :as r]
   [spark.core :as spark]))

(defmacro defnc [& body] `(r/defnc ~@body))
(defmacro $ [type & args] `(r/$ ~type ~@args))
(defmacro <> [& children] `(r/<> ~@children))
(defmacro use-state [& body] `(r/use-state ~@body))
(defmacro use-effect [& body] `(r/use-effect ~@body))
(defmacro use-memo [& body] `(r/use-memo ~@body))
(defmacro create-context [& body] `(r/create-context ~@body))
(defmacro use-context [& body] `(r/use-context ~@body))
(defmacro provider [& body] `(r/provider ~@body))

(defmacro def-page  [sym opts]
  (let [opts (spark/complete-opts opts sym "page")]
    `(def ~sym (reg-page ~opts))))

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
                 (update opts :wrap conj `(r/memo
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

(defmacro def-ui-showcase [id-keyword component]
  (when (= :dev (:shadow.build/mode &env))
    `(reg-showcase ~id-keyword
                   {:component (fn [] ~component)
                    :code '~component})))

(defn- conform-style-value [v]
  (cond
    (vector? v)  (->> v (map conform-style-value) (str/join " "))
    (string? v)  v
    (keyword? v) (name v)
    :else        v))

(defn- conform-style [styles]
  (reduce (fn [styles [k v]]
            (assoc styles k (conform-style-value v)))
          {} styles))

(defn- html-element
  ([element style-and-children]
   (html-element element style-and-children nil nil))
  ([element style-and-children extra-class extra-style]
   (let [[style children] (if (-> style-and-children first map?)
                            [(first style-and-children) (rest style-and-children)]
                            [nil style-and-children])

         style (if extra-style
                 (merge extra-style style)
                 style)

         props         {}
         [props style] (if-let [k (-> style :key)]
                         [(assoc props :key k) (dissoc style :key)]
                         [props style])
         [props style] (if-let [id (-> style :id)]
                         [(assoc props :id id) (dissoc style :id)]
                         [props style])
         [props style] (if-let [k (-> style :ref)]
                         [(assoc props :ref k) (dissoc style :ref)]
                         [props style])
         [props style] (if-let [v (-> style :tab-index)]
                         [(assoc props :tab-index v) (dissoc style :tab-index)]
                         [props style])
         [props style] (if-let [v (-> style :on-click)]
                         [(assoc props :onClick v) (dissoc style :on-click)]
                         [props style])
         [props style] (if-let [v (-> style :onKeyDown)]
                         [(assoc props :onKeyDown v) (dissoc style :onKeyDown)]
                         [props style])
         [props style] (if-let [v (-> style :onKeyPress)]
                         [(assoc props :onKeyPress v) (dissoc style :onKeyPress)]
                         [props style])
         [props style] (if-let [class (if extra-class
                                        (str extra-class " " (-> style :class))
                                        (-> style :class))]
                         [(assoc props :className class) (dissoc style :class)]
                         [props style])
         props         (assoc props :style (conform-style style))]
     `($ ~element ~props ~@children))))

(defmacro div [& style-and-children]
  (html-element :div style-and-children))

(defmacro span [& style-and-children]
  (html-element :span style-and-children))

(defmacro center [& style-and-children]
  (html-element :span style-and-children "center" nil))

(defmacro center-div [& style-and-children]
  (html-element :div style-and-children "center" nil))

(defmacro center-text [& style-and-children]
  (html-element :div style-and-children "center-text" nil))

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
     ($ ~component
        {:key      item#
         ~item-key item#})))

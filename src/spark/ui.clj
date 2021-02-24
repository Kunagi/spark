(ns spark.ui
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]

   [spark.react :as r]
   [spark.core :as spark]))


(defmacro defnc [& body ] `(r/defnc ~@body))
(defmacro $ [type & args] `(r/$ ~type ~@args))
(defmacro <> [& children] `(r/<> ~@children))
(defmacro use-state [& body] `(r/use-state ~@body))
(defmacro use-effect [& body] `(r/use-effect ~@body))
(defmacro use-memo [& body] `(r/use-memo ~@body))

(defmacro def-page [sym opts] `(spark/def-page ~sym ~opts))

(defmacro def-ui [type params & body]
  (let [[docstring params body] (if (string? params)
                                  [params (first body) (rest body)]
                                  [nil params body])

        params (if (-> params first map?)
                 params
                 [{:keys params}])

        opts? (map? (first body))
        opts (if opts?
               (first body)
               {})
        body (if opts?
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

        body (if (seq lets)
               `((let [~@lets]
                   ~@body))
               body)]
    `(defnc
       ~type
       ~@(when docstring [docstring])
       ~params
       ~@body)))


(defmacro def-ui-test [[sym & requires] & examples]
  (when (= :dev (:shadow.build/mode &env))
    (let [symbol-name (-> sym name )
          calling-namespace-name (name (ns-name *ns*))
          examples examples
          devcard {:id (str calling-namespace-name "/" symbol-name)
                   :namespace calling-namespace-name
                   :symbol symbol-name}
          examples (mapv (fn [example]
                           {:code (with-out-str (pprint example))
                            :f `(fn []
                                  ~example)})
                         examples)]
      `(reg-devcard
        (assoc ~devcard
               :type :ui
               :examples ~examples)))))


(defn- conform-style-value [v]
  (cond
    (vector? v)(->> v (map conform-style-value) (str/join " "))
    (string? v) v
    (keyword? v) (name v)
    :else v))


(defn- conform-style [styles]
  (reduce (fn [styles [k v]]
            (assoc styles k (conform-style-value v)))
          {} styles))

(defn- html-element [element style-and-children]
  (let [[style children] (if (-> style-and-children first map?)
                           [(first style-and-children) (rest style-and-children)]
                           [nil style-and-children])
        props {}
        [props style] (if-let [k (-> style :key)]
                        [(assoc props :key k) (dissoc style :key)]
                        [props style])
        [props style] (if-let [id (-> style :id)]
                        [(assoc props :id id) (dissoc style :id)]
                        [props style])
        [props style] (if-let [class (-> style :class)]
                        [(assoc props :className class) (dissoc style :class)]
                        [props style])
        props (assoc props :style (conform-style style))]
    `($ ~element ~props ~@children)))

(defmacro div [& style-and-children]
  (html-element :div style-and-children))

(defmacro span [& style-and-children]
  (html-element :span style-and-children))


(defmacro grid [grid-template-columns & style-and-children]
  (let [[style children] (if (-> style-and-children first map?)
                           [(first style-and-children) (rest style-and-children)]
                           [nil style-and-children])
        style (-> style
                  (merge {:display "grid"
                          :grid-template-columns grid-template-columns})
                  conform-style)]
    `($ :div
        {:style ~style}
        ~@children)))


(defn- div-class [class children]
  `($ :div {:className ~class} ~@children))

(defmacro stack [& children] (div-class "stack" children))
(defmacro stack-0 [& children] (div-class "stack-0" children))
(defmacro stack-1 [& children] (div-class "stack-1" children))
(defmacro stack-2 [& children] (div-class "stack-2" children))
(defmacro stack-3 [& children] (div-class "stack-3" children))
(defmacro stack-4 [& children] (div-class "stack-4" children))
(defmacro stack-5 [& children] (div-class "stack-5" children))

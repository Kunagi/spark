(ns spark.ui
  (:require
   [clojure.pprint :refer [pprint]]
   [spark.react :as r]
   [clojure.string :as str]))


(defmacro defnc [& body ] `(r/defnc ~@body))
(defmacro $ [type & args] `(r/$ ~type ~@args))
(defmacro <> [& children] `(r/<> ~@children))
(defmacro use-state [& body] `(r/use-state ~@body))
(defmacro use-effect [& body] `(r/use-effect ~@body))



(defmacro devcard [sym & examples]
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
             :examples ~examples))))


(defn- conform-style-value [v]
  (cond
    (vector? v)(->> v (map conform-style-value) (str/join " "))
    (string? v) v
    (keyword? v) (name v)
    (number? v) (str v"px")
    :else v))


(defn- conform-style [styles]
  (reduce (fn [styles [k v]]
            (assoc styles k (conform-style-value v)))
          {} styles))


(defmacro div [& style-and-children]
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
    `($ :div ~props ~@children)))


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

(ns spark.ui.styles
  (:require
   [clojure.string :as str]
   ["@material-ui/core/styles" :as mui-styles]
   ))

(defn app-styles [theme]
  {
   :.center {:display :grid :place-items "center"}

   :.grid-0 {:display :grid}
   :.grid-1 {:display :grid :grid-gap (-> theme (.spacing 1))}
   :.grid-2 {:display :grid :grid-gap (-> theme (.spacing 2))}
   :.grid-3 {:display :grid :grid-gap (-> theme (.spacing 3))}
   :.grid-4 {:display :grid :grid-gap (-> theme (.spacing 4))}
   :.grid-5 {:display :grid :grid-gap (-> theme (.spacing 5))}

   :.stack   {:display :grid :grid-gap (-> theme (.spacing 1))}
   :.stack-0 {:display :grid}
   :.stack-1 {:display :grid :grid-gap (-> theme (.spacing 1))}
   :.stack-2 {:display :grid :grid-gap (-> theme (.spacing 2))}
   :.stack-3 {:display :grid :grid-gap (-> theme (.spacing 3))}
   :.stack-4 {:display :grid :grid-gap (-> theme (.spacing 4))}
   :.stack-5 {:display :grid :grid-gap (-> theme (.spacing 5))}

   ".flex"       {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 1) (/ 2)) "px")}
   ".flex > *"   {:margin (str (-> theme (.spacing 1) (/ 2)) "px")}
   ".flex-0"     {:display :flex :flex-wrap "wrap"}
   ".flex-1"     {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 1) (/ 2)) "px")}
   ".flex-1 > *" {:margin (str (-> theme (.spacing 1) (/ 2)) "px")}
   ".flex-2"     {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 2) (/ 2)) "px")}
   ".flex-2 > *" {:margin (str (-> theme (.spacing 2) (/ 2)) "px")}
   ".flex-3"     {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 3) (/ 2)) "px")}
   ".flex-3 > *" {:margin (str (-> theme (.spacing 3) (/ 2)) "px")}
   ".flex-4"     {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 4) (/ 2)) "px")}
   ".flex-4 > *" {:margin (str (-> theme (.spacing 4) (/ 2)) "px")}
   ".flex-5"     {:display :flex :flex-wrap "wrap"
                  :margin  (str "-" (-> theme (.spacing 5) (/ 2)) "px")}
   ".flex-5 > *" {:margin (str (-> theme (.spacing 5) (/ 2)) "px")}

   "a.Link--no-styles" {:text-decoration :none
                        :color           :unset
                        :display         :block}
   })


(defn- conform-style-value [v]
  (cond
    (vector? v)  (->> v (map conform-style-value) (str/join " "))
    (string? v)  v
    (keyword? v) (name v)
    :else        v))


(defn- conform-style [styles]
  (reduce (fn [styles [k v]]
            (if (and (string? k) (str/starts-with? "&" k))
              (assoc styles k (conform-style v))
              (assoc styles k (conform-style-value v))))
          {} styles))

(defn- conform-styles-selector [s]
  (cond
    (keyword? s)              (str "& " (name s))
    (str/starts-with? s "& ") s
    :else                     (str "& " s)))

(defn- conform-styles [styles]
  (reduce (fn [styles [k v]]
            (assoc styles
                   (conform-styles-selector k)
                   (conform-style v)))
          {} styles))


(defn adapt-theme [theme]
  (-> theme
      (or {})
      clj->js
      mui-styles/createMuiTheme
      mui-styles/responsiveFontSizes))

(defn adapt-styles [styles]
  (fn [theme]
    (conform-styles
     (if styles
       (merge (app-styles theme) (styles theme))
       (app-styles theme)))))

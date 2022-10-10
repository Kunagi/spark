(ns spark.ui.styles
  (:require
   [clojure.string :as str]
   ["@mui/material/styles" :as mui-styles]
   [camel-snake-kebab.core :as csk]
   ))

(defn app-styles [theme]
  {:.center {:display :grid :place-items "center"}
   :.center-text {:text-align "center"}

   :.right {:display :grid :place-items "end"}

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
                  :margin (-> theme (.spacing -0.5))}
   ".flex > *"   {:margin (-> theme (.spacing 0.5))}
   ".flex-0"     {:display :flex :flex-wrap "wrap"}
   ".flex-1"     {:display :flex :flex-wrap "wrap"
                  :margin (-> theme (.spacing 0.5) )}
   ".flex-1 > *" {:margin (-> theme (.spacing 0.5))}
   ".flex-2"     {:display :flex :flex-wrap "wrap"
                  :margin (-> theme (.spacing -1) )}
   ".flex-2 > *" {:margin (-> theme (.spacing 1))}
   ".flex-3"     {:display :flex :flex-wrap "wrap"
                  :margin (-> theme (.spacing -1.5) )}
   ".flex-3 > *" {:margin (-> theme (.spacing 1.5))}
   ".flex-4"     {:display :flex :flex-wrap "wrap"
                  :margin (-> theme (.spacing -2))}
   ".flex-4 > *" {:margin (-> theme (.spacing 2))}
   ".flex-5"     {:display :flex :flex-wrap "wrap"
                  :margin  (-> theme (.spacing -2.5) )}
   ".flex-5 > *" {:margin (-> theme (.spacing 2.5))}

   "a.Link--no-styles" {:text-decoration :none
                        :color           :unset
                        ;; :display         :block
                        }

   ".CursorPointer" {:cursor :pointer}})

(def attr-with-px?
  #{:padding "padding"
    :padding-top "padding-top"
    :padding-bottom "padding-bottom"
    :padding-left "padding-left"
    :padding-right "padding-right"

    :margin "margin"
    :margin-top "margin-top"
    :margin-bottom "margin-bottom"
    :margin-left "margin-left"
    :margin-right "margin-right"

    :grid-gap "grid-gap"
    :gap "gap"})

(defn- conform-style-value [v attr]
  (cond
    (vector? v)                (->> v (map #(conform-style-value % attr)) (str/join " "))
    (string? v)                v
    (keyword? v)               (name v)
    (= 0 v)                    v
    (and (number? v)
         (attr-with-px? attr)) (str v "px")
    :else                      v))

(defn- ->camelCase-last [s]
  (let [strings (str/split s #"\s")
        ]
    (if (-> strings count (= 1))
      (csk/->camelCase (first strings))
      (str/join " "
                (conj (butlast strings)
                      (csk/->camelCase (last strings)))))))
(defn- conform-style-key [k camel?]
  (if camel?
    (if (keyword? k)
      (-> k name ->camelCase-last)
      (->camelCase-last k))
    (if (keyword? k)
      (-> k name)
      (str k))))

(defn- conform-style [styles camel?]
  (reduce (fn [styles [k v]]
            (if (and (string? k) (str/starts-with? "&" k))
              (assoc styles (conform-style-key k camel?) (conform-style v camel?))
              (assoc styles (conform-style-key k camel?) (conform-style-value v k))))
          {} styles))

(defn- conform-styles-selector [s camel?]
  (conform-style-key
   (cond
     (keyword? s)              (str "& " (name s))
     (str/starts-with? s "& ") s
     :else                     (str "& " s))
   camel?))

(defn conform-styles [styles camel?]
  (reduce (fn [styles [k v]]
            (if (map? v)
              (assoc styles
                     (conform-styles-selector k camel?)
                     (conform-style v camel?))
              (assoc styles
                     (conform-style-key k camel?)
                     (conform-style-value v k))))
          {} styles))

(defn adapt-theme [theme]
  (-> theme
      (or {})
      clj->js
      ;; mui-styles/adaptV4Theme
      mui-styles/createTheme
      mui-styles/responsiveFontSizes))

(defn adapt-styles [styles]
  (fn [theme]
    (conform-styles
     (if styles
       (merge (app-styles theme) (styles theme))
       (app-styles theme))
     false)))

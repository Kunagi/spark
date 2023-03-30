(ns spark.ui.styles
  (:require
   ["@mui/material/styles" :as mui-styles]
   [kunagi.mui.api :as ui]))

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


(defn adapt-theme [theme]
  (-> theme
      (or {})
      clj->js
      ;; mui-styles/adaptV4Theme
      mui-styles/createTheme
      mui-styles/responsiveFontSizes))

(defn adapt-styles [styles]
  (fn [theme]
    (ui/conform-styles
     (if styles
       (merge (app-styles theme) (styles theme))
       (app-styles theme))
     false)))

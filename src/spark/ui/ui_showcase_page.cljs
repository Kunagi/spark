(ns spark.ui.ui-showcase-page
  (:require
   ["@mui/material" :as mui]
   [spark.ui :as ui :refer [$ def-page def-ui]]
   [spark.ui.showcase :as showcase]))

(def use-showcases (ui/atom-hook showcase/SHOWCASES))

(def-ui Showcase [id]
  (let [showcases (use-showcases)
        showcase (get showcases id)
        component-factory (-> showcase :component)
        component (component-factory)
        code (-> showcase :code)
        ]
    (ui/stack
     (ui/div
      (str id))
     ($ mui/Divider)
     (ui/div
      {:display :grid
       :border "1px dotted grey"
       :padding 5}
      component)
     ($ mui/Divider)
     (ui/data code)
     )))

(def-ui Navigator []
  (let [showcases (use-showcases)]
    (ui/stack
     (for [showcase (->> showcases vals (sort-by :showcase/id))]
       (let [id (-> showcase :showcase/id)]
         ($ ui/Link
            {:key id
             :to [(str "ui-showcase?ns=" (namespace id) "&name=" (name id))]}
            (str id)))))))

(def-ui PageContent []
  (let [showcase-ns (ui/use-url-param :ns)
        showcase-name (ui/use-url-param :name)]
    (if (and showcase-ns showcase-name)
      ($ Showcase {:id (keyword showcase-ns showcase-name)})
      ($ Navigator))))

(def-page ui-showcase-page
  {:path             ["ui-showcase"]
   :title "UI Components Showcase"
   :content          PageContent
   })

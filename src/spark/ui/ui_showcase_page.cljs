(ns spark.ui.ui-showcase-page
  (:require
   ["@material-ui/core" :as mui]
   [clojure.edn :as edn]
   [helix.core :as helix.core]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.ui :as ui :refer [def-ui def-page $]]
   [spark.firebase-functions :as functions]
   [spark.ui.showcase :as showcase]

   [spark.googlemaps :as maps]

   [ui.hgui :as hgui]

   [domain.facebook :as facebook]

   ))

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

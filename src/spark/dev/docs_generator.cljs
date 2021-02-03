(ns spark.dev.docs-generator
  (:require
   [clojure.string :as str]
   [spark.dev.adoc :as adoc]
   [spark.models :as models]
   [spark.browser :as browser]))


;;;
;;; col
;;;

(defn col-section [col]
  (let [doc (-> col :doc)]
    (str (adoc/section "=" (models/col-path col))
         (adoc/section (-> col :dev-doc))
         (adoc/section (-> doc :dev-doc))
         (adoc/table "cols=2*"
                     [["Collection Symbol" (adoc/monospace (-> col :model/id))]
                      ["Document Symbol" (adoc/monospace (-> doc :model/id))]] )

         )))

(comment
  (def col (-> @models/MODELS
               (get "base.benutzer/Benutzers")))
  (def col (-> (models/models-by-constructor models/Col)
               first)))

(defn cols-sections []
  (->> (models/models-by-constructor models/Col)
       (sort-by :model/symbol)
       (map col-section)
       (str/join "\n")))


;;;
;;; Shadow-CLJS hook
;;;


(defn ^:dev/after-load after-load []
  (browser/send-text-to-url-via-img
   "http://localhost:8000/write-cols-doc?"
   (cols-sections))
  )

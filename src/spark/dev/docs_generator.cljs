(ns spark.dev.docs-generator
  (:require
   [clojure.string :as str]
   [spark.browser :as browser]
   [spark.core :as spark]
   [spark.dev.adoc :as adoc]))


;;;
;;; cols
;;;

(defn doc-section [Doc]
  (let [doc (spark/schema-opts Doc)]
    (str (adoc/section "=" (spark/doc-schema-col-path Doc))
         (adoc/section (-> doc :dev/doc))
         (adoc/table "cols=2*"
                     [["Symbol" (adoc/monospace (-> doc :doc-schema/id))]]))))



(defn docs-sections []
  (->> [] ;; FIXME (models/models-by-constructor models/Col)
       (sort-by :model/symbol)
       (map doc-section)
       (str/join "\n")))

;;;
;;; commands
;;;


(defn command-section [cmd]
  (str (adoc/section "=" (-> cmd :model/symbol))
       (adoc/section (-> cmd :dev-doc))
       (adoc/table "cols=2*"
                   [["Symbol" (adoc/monospace (-> cmd :model/id))]
                    ["Label" (-> cmd :label)]])))

(defn commands-sections []
  (->> [] ;; FIXME (models/models-by-constructor models/Command)
       (sort-by :model/symbol)
       (map command-section)
       (str/join "\n")))

;;;
;;; Shadow-CLJS hook
;;;

(defn- send-doc [type adoc]
  (browser/send-text-to-url-via-img
   (str "http://localhost:8000/write-" type "-doc?")
   adoc))

(defn ^:dev/after-load after-load []
  (send-doc "docs" (docs-sections))
  (send-doc "commands" (commands-sections))
  )

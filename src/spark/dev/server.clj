(ns spark.dev.server
  (:require
   [clojure.pprint :refer [pprint]]
   [ring.util.codec :as codec]))


(defn write-cols-doc [adoc]
  (let [path "dev-doc/cols.adoc"]
    (print "write: " path)
    (spit path adoc)
    {:status 201
     :body (str "written: " path)}))


(defn http-handler [{:keys [uri query-string] :as req}]
  (case uri
    "/write-cols-doc" (write-cols-doc (codec/url-decode query-string "utf-8"))
    (do
      (pprint req)
      {:status 201 :body "Spark Dev Server"})
    )
  )

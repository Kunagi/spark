(ns spark.dev.server
  (:require
   [clojure.pprint :refer [pprint]]
   [ring.util.codec :as codec]))


(defn write-dev-doc [type adoc]
  (let [path (str "dev-doc/" type ".adoc")]
    ;; (print "write: " path)
    (spit path adoc)
    {:status 201
     :body (str "written: " path)}))


(defn http-handler [{:keys [uri query-string] :as req}]
  (case uri

    "/write-cols-doc"
    (write-dev-doc "cols" (codec/url-decode query-string "utf-8"))

    "/write-commands-doc"
    (write-dev-doc "commands" (codec/url-decode query-string "utf-8"))

    (do
      ;; (pprint req)
      {:status 201 :body "Spark Dev Server"})
    )
  )

(ns spark.pages
  (:require
   [clojure.string :as str]
   [camel-snake-kebab.core :as csk]
   [spark.logging :refer [log]]
   [spark.core :as spark]
   ))

(defonce PAGES (atom {}))

(defn pages []
  (-> @PAGES vals))

(defn pages-in-router-order []
  (->> (pages)
       (sort-by :router-path)
       reverse))

(defn- coerce-page-path [thing]
  (if (string? thing)

    thing

    (->> thing
         (map (fn [path-element]
                (cond
                  (keyword? path-element)
                  (str ":" (-> path-element csk/->camelCaseString))

                  (spark/doc-schema? path-element)
                  (-> path-element spark/doc-schema-router-param)

                  (spark/subdoc-schema? path-element)
                  (-> path-element spark/subdoc-schema-router-param)

                  :else
                  (str path-element))))
         (str/join "/")
         (str "/ui/"))))

(comment
  (coerce-page-path "/ui/book/:book")
  (coerce-page-path ["book" :book-id])
  (coerce-page-path []))

(defn reg-page [page]
  ;; (log ::reg-page
  ;;      :page page)
  (let [k (keyword (-> page :page/namespace) (-> page :page/symbol))
        page (assoc page
                    :id k
                    :router-path (coerce-page-path (-> page :path)))]
    (swap! PAGES assoc k page)
    page))

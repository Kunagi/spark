(ns spark.debug-ui
  (:require
   [kunagi.utils.debug :as debug]
   [spark.ui :as ui :refer [def-ui $]]))

(def use-active (ui/atom-hook debug/ACTIVE))

(def use-items (ui/atom-hook debug/ITEMS))

(def-ui ItemsCounts []
  (let [items (use-items)]
    (ui/data (->> items
                  (map (fn [[k v]]
                         [k (count v)]))))))

(ns commons.domain-api
  (:require
   [malli.core :as m]
   [malli.error :as me]))


(def PropertyKey
  keyword?)

(def PropertyPath
  [:vector PropertyKey])

(def EntityDef
  [:map
   [:id keyword?]])

(def DocDef
  EntityDef
  #_(into EntityDef
        [[:firestore/collection string?]]))

(defn doc-id [doc]
  (let [id (get doc :firestore/id)]
    (when-not id (throw (ex-info "Missing :firestore/id in document."
                                 {:doc doc})))
    id))

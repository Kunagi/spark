(ns projekt.issues-page
  (:require

   ["@mui/material" :as mui]


   [spark.ui :as ui :refer [def-page def-ui $]]

   [projekt.projekt :as projekt]
   ))


(def-ui IssuesCards [type projekt]
  {:from-context [projekt]}
  (let [issues (projekt/issues projekt type)]
    ($ ui/Stack
       ($ :div (projekt/issue-types-label type))
       ($ ui/Flexbox
          (for [issue issues]
            ($ ui/DocFieldCard
               {:key issue
                :doc projekt
                :field {:id (projekt/issues-attr-id type)
                        :multiline? true}
                :value-filter (fn [_] issue)})
            #_($ ui/SimpleCard
                 {:key issue}
                 issue))))))


(def-ui Issues []
  (ui/stack
   ($ IssuesCards {:type :bug})
   ($ IssuesCards {:type :debt})
   ($ IssuesCards {:type :idea})))


(def-page issues-page
  {:path "/ui/projekt/issues"
   :max-width false
   :content Issues
   :update-context (fn [context]
                     (assoc context :projekt (ui/use-singleton-doc projekt/Projekt)))})

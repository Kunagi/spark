(ns kunagi.mui.core
  (:require-macros [kunagi.mui.core])
  (:require
   ["react" :as react]
   ["react-dom" :as rdom]
   [helix.core :as helix]
   [helix.hooks :as helix-hooks]))

(def create-ref react/createRef)
(def memo helix/memo)

(defn atom-hook_
  ([ATOM]
   (atom-hook_ ATOM identity))
  ([ATOM transformator]
   (fn use-atom []
     (let [[value set-value] (helix-hooks/use-state @ATOM)
           watch-key (random-uuid)]

       (helix-hooks/use-effect
        :once
        (set-value @ATOM)
        (add-watch ATOM watch-key
                   (fn [_k _r ov nv]
                     (when-not (= ov nv)
                       (set-value nv))))
        #(remove-watch ATOM watch-key))

       (transformator value)))))

(def atom-hook atom-hook_ #_(memoize atom-hook_))

(defn mount [component element-id]
  (assert (string? element-id))
  (assert component)
  (rdom/render component
               (js/document.getElementById element-id)))

(ns spark.react
  (:require-macros [spark.react :refer [use-state use-effect]])
  (:require
   [helix.hooks :as helix-hooks]))


(defn atom-hook
  ([ATOM]
   (atom-hook ATOM identity))
  ([ATOM transformator]
   (fn use-atom []
     (let [[value set-value] (helix-hooks/use-state @ATOM)
           watch-key (random-uuid)]

       (use-effect
        :once
        (set-value @ATOM)
        (add-watch ATOM watch-key
                   (fn [_k _r ov nv]
                     (when-not (= ov nv)
                       (set-value nv))))
        #(remove-watch ATOM watch-key))

       (transformator value)))))

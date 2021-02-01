(ns commons.firestore-hooks
  (:require
   [clojure.spec.alpha :as s]
   [helix.hooks :as hooks]

   [commons.logging :refer [log]]
   [commons.firestore :as fs]))


(defn doc-atom [path]
  (log ::doc-atom :path path)
  (s/assert ::fs/opt-path path)
  (let [DATA (atom nil)
        ref (fs/ref path)]
    (when ref
      (-> ref
          (.onSnapshot (fn [doc-snapshot]
                         (log ::doc-snapshot-received
                              :collection path
                              :snapshot doc-snapshot)
                         (reset! DATA (fs/wrap-doc doc-snapshot)))
                       (fn [^js error]
                         (log ::doc-atom-error
                              :path path
                              :exception error)))))
    DATA))


(defn col-atom [path]
  (log ::col-atom
       :path path)
  (s/assert ::fs/opt-path path)
  (let [DATA (atom nil)
        ref (fs/ref path)]
    (log ::subscribing
         :collection path)
    (when ref
      (-> ref
          (.onSnapshot (fn [^js query-col-snapshot]
                         (log ::query-snapshot-received
                              :collection path
                              :count (-> query-col-snapshot .-docs count)
                              :snapshot query-col-snapshot)
                         (->> ^js query-col-snapshot
                              .-docs
                              (map fs/wrap-doc)
                              (reset! DATA)))
                       (fn [^js error]
                         (js/console.error "Loading collection failed" path error)))))
    DATA))


(defonce SUBS (atom {}))

(defn doc-sub [path]
  (s/assert ::fs/opt-path path)
  (if-let [DATA (get @SUBS path)]
    DATA
    (let [DATA (doc-atom path)]
      (swap! SUBS assoc path DATA)
      DATA)))


(defn col-sub [path]
  (s/assert ::fs/opt-path path)
  (if-let [DATA (get @SUBS path)]
    DATA
    (let [DATA (col-atom path)]
      (swap! SUBS assoc path DATA)
      DATA)))


(defn use-col
  "React hook for a collection."
  [path]
  (log ::use-col
       :path path)
  (s/assert ::fs/opt-path path)
  (let [DATA (col-sub path)
        [docs set-docs] (hooks/use-state @DATA)
        watch-ref (random-uuid)]

    (hooks/use-effect
     :always
     (set-docs @DATA)
     (add-watch DATA
                watch-ref
                (fn [_ _ _ nv]
                  (set-docs nv)))

     #(remove-watch DATA watch-ref))

    docs))


#_(defn use-cols-union
  "React hook for a union of collections."
  [paths]
  (log ::use-cols-union
       :paths paths)
  (let [DATAs (mapv #(col-sub %) paths)
        create-union (fn []
                       (reduce (fn [ret DATA]
                                 (when ret
                                   (when-let [data @DATA]
                                     (into ret data))))
                               #{} DATAs))
        [docs set-docs] (hooks/use-state (create-union))
        watch-ref (random-uuid)]

    (hooks/use-effect
     :always
     (set-docs (create-union))
     (doseq [DATA DATAs]
       (add-watch DATA
                  watch-ref
                  (fn [_ _ _ nv]
                    (set-docs (create-union)))))

     #(doseq [DATA DATAs]
        (remove-watch DATA watch-ref)))

    docs))


(defn use-cols-union
  "React hook for a union of collections."
  [paths]
  (log ::use-cols-union
       :paths paths)
  (->> paths
       (reduce (fn [ret path]
                 (let [docs (use-col path)]
                   (reduce (fn [ret doc]
                             (assoc ret (-> doc  :id) doc))
                           ret docs)))
               {})
       vals))


(defn use-doc
  "React hook for a document."
  [path]
  (s/assert ::fs/opt-path path)
  (let [DATA (doc-sub path)
        [doc set-doc] (hooks/use-state @DATA)
        watch-ref (random-uuid)]

    (hooks/use-effect
     :always
     (set-doc @DATA)
     (add-watch DATA
                watch-ref
                (fn [_ _ _ nv]
                  (set-doc nv)))

     #(remove-watch DATA watch-ref))

    doc))

(ns spark.firestore-hooks
  (:require
   [clojure.spec.alpha :as s]

   [spark.logging :refer [log]]
   [spark.firestore :as fs]))


;; (defn doc-atom [path]
;;   (log ::doc-atom :path path)
;;   (s/assert ::fs/opt-path path)
;;   (when path
;;     (let [DATA (atom nil)
;;           ref (fs/ref path)]
;;       (-> ref
;;            (.onSnapshot (fn [doc-snapshot]
;;                           (log ::doc-snapshot-received
;;                                :collection path
;;                                :snapshot doc-snapshot)
;;                           (reset! DATA (fs/wrap-doc doc-snapshot)))
;;                         (fn [^js error]
;;                           (log ::doc-atom-error
;;                                :path path
;;                                :exception error))))
;;       DATA)))


(defn col-atom [path]
  (log ::col-atom
       :path path)
  (s/assert ::fs/opt-path path)
  (when path
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
      DATA)))


(defonce SUBS (atom {}))


;; (defn doc-sub [path]
;;   (s/assert ::fs/opt-path path)
;;   (when path
;;     (if-let [DATA (get @SUBS path)]
;;       DATA
;;       (let [DATA (doc-atom path)]
;;         (swap! SUBS assoc path DATA)
;;         DATA))))



(defn col-sub [path]
  (s/assert ::fs/opt-path path)
  (when path
    (if-let [DATA (get @SUBS path)]
      DATA
      (let [DATA (col-atom path)]
        (swap! SUBS assoc path DATA)
        DATA))))

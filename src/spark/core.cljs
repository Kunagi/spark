(ns spark.core
  (:require-macros [spark.core :refer [defn-test]]))

;;;
;;; test registry
;;;

(defonce TESTS (atom {}))

(defn reg-test [test]
  ;; TODO disable for production
  (swap! TESTS assoc (-> test :id) test))

;;;
;;; test helpers
;;;


;;;
;;;
;;;

(defn opts [map-schema]
  (-> map-schema second))

(defn-test [opts expect]
  (expect {:opt "value"}
          (opts [:map {:opt "value"}])))


(defn type-of? [type thing]
  (boolean
   (and (vector? thing)
        (when-let [opts (opts thing)]
          (and (map? opts)
               (opts (keyword (name type) "id")))))))

(defn-test [type-of? expect]
  (expect true
          (type-of? :doc [:map {:doc/id "some.Doc"}]))
  (type-of? :doc [:map {:something :else}]))

;;;
;;; doc
;;;

(defn doc? [thing]
  (type-of? :doc thing))

(defn doc-col-path [doc]
  (-> doc opts :firestore/collection))

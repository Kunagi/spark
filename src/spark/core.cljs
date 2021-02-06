(ns spark.core
  (:require-macros [spark.core :refer [def-test]])
  (:require
   [spark.firestore :as firestore]))

;;;
;;; test registry
;;;

(defonce TESTS (atom {}))

(defn reg-test [test]
  (swap! TESTS assoc (-> test :id) test))

;;;
;;;
;;;

(defn schema-opts [map-schema]
  (-> map-schema second))

(def-test [schema-opts expect]
  (expect {:opt "value"}
          (schema-opts [:map {:opt "value"}])))


(defn schema-type-of? [type thing]
  (boolean
   (and (vector? thing)
        (when-let [opts (schema-opts thing)]
          (and (map? opts)
               (get opts (keyword (name type) "id")))))))

(def-test [schema-type-of? expect]
  (expect true
          (schema-type-of? :doc-schema [:map {:doc-schema/id "some.Doc"}]))
  (schema-type-of? :doc-schema [:map {:something :else}]))


;;;
;;; Doc
;;;

(defn doc-schema? [thing]
  (schema-type-of? :doc-schema thing))

(defn doc-schema-col-path [Doc]
  (-> Doc schema-opts :firestore/collection))

(defn doc-schema-id-generator [Doc]
  (or (-> Doc schema-opts :spark/id-generator)
      (fn [_context] (-> (random-uuid) str))))

(defn new-doc-id [Doc context]
  (-> Doc
      doc-schema-id-generator
      (apply context)))

(defn new-doc [Doc values]
  (let [id (or (-> values :id) (new-doc-id Doc {:values values}))]
    (assoc values
           :firestore/id id
           :firestore/path (str (doc-schema-col-path Doc) "/" id)
           :id id
           :ts-created [:db/timestamp]
           :ts-updated [:db/timestamp])))

;;;
;;; Subdoc
;;;

(defn subdoc-schema? [thing]
  (schema-type-of? :subdoc-schema thing))

(defn new-subdoc-id []
  (-> (random-uuid) str))
;;;
;;;
;;;

(defn save-doc> [doc]
  (firestore/save-doc> doc))


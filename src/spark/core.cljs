(ns spark.core
  (:require-macros [spark.core :refer [def-test]]
                   [clojure.string :as str])
  (:require
   [clojure.string :as str]
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
;;; Field
;;;

(defn field-schema? [thing]
  (schema-type-of? :field-schema thing))

(defn assert-field-schema [Field]
  ;; FIXME dev only
  (when-not (field-schema? Field)
    (throw (ex-info "field schema expected"
                    {:value Field}))))

(defn field-schema-as-form-field [Field]
  (assert-field-schema Field)
  (schema-opts Field))

;;;
;;; Doc
;;;

(defn doc-schema? [thing]
  (schema-type-of? :doc-schema thing))

(defn assert-doc-schema [Doc]
  ;; FIXME dev only
  (when-not (doc-schema? Doc)
    (throw (ex-info "doc schema expected"
                    {:value Doc}))))

(defn doc-schema-col-path [Doc]
  (assert-doc-schema Doc)
  (-> Doc schema-opts :firestore/collection))

(defn doc-schema-singleton-doc-path [Doc]
  (assert-doc-schema Doc)
  ["singletons" (let [opts (schema-opts Doc)]
                  (-> opts :firestore/doc-id)
                  (-> opts :doc-schema/symbol str/lower-case))])

(defn doc-schema-id-generator [Doc]
  (assert-doc-schema Doc)
  (or (-> Doc schema-opts :spark/id-generator)
      (fn [_context] (-> (random-uuid) str))))

(defn new-doc-id [Doc context]
  (assert-doc-schema Doc)
  (-> Doc
      doc-schema-id-generator
      (apply context)))

(defn new-doc [Doc values]
  (assert-doc-schema Doc)
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

(defn subdoc-schema-id-generator [Doc]
  (or (-> Doc schema-opts :spark/id-generator)
      (fn [_context] (-> (random-uuid) str))))

(defn new-subdoc-id [Subdoc context]
  (-> Subdoc
      subdoc-schema-id-generator
      (apply context)))


;;;
;;; cmd
;;;

(defn cmd? [thing]
  (and (map? thing)
       (get thing(keyword "cmd" "id"))))

(defn assert-cmd [cmd]
  ;; FIXME dev only
  (when-not (cmd? cmd)
    (throw (ex-info "cmd expected"
                    {:value cmd}))))

(defn cmd-label [cmd]
  (assert-cmd cmd)
  (or (-> cmd :label)
      (-> cmd :cmd/symbol)))

;;;
;;; page
;;;

(defn page? [thing]
  (and (map? thing)
       (get thing(keyword "page" "id"))))

(defn assert-page [page]
  ;; FIXME dev only
  (when-not (page? page)
    (throw (ex-info "page expected"
                    {:value page}))))

;;;
;;; spa
;;;

(defn spa? [thing]
  (and (map? thing)
       (get thing(keyword "spa" "id"))))

(defn assert-spa [spa]
  ;; FIXME dev only
  (when-not (spa? spa)
    (throw (ex-info "spa expected"
                    {:value spa}))))

(defn spa-pages [spa]
  (assert-spa spa)
  (-> spa :pages))

;;;
;;;
;;;

(defn save-doc> [doc]
  (firestore/save-doc> doc))


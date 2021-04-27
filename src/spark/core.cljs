(ns spark.core
  (:require-macros [spark.core :refer [def-test]])
  (:require
   [clojure.string :as str]
   [spark.firestore :as firestore]
   [camel-snake-kebab.core :as csk]
   ))

;;;
;;; test registry
;;;

(defonce TESTS (atom {}))

(defn reg-test [test]
  (swap! TESTS assoc (-> test :id) test))

(defonce DEFS (atom {}))

(defn reg-def [type id thing]
  (swap! DEFS assoc-in [type id] thing))

;;;
;;;
;;;

(defn schema-opts [map-schema]
  (-> map-schema second))

(def-test [schema-opts expect]
  (expect {:opt "value"}
          (schema-opts [:map {:opt "value"}])))

(comment
  (macroexpand-1 '(def-test [schema-opts expect]
                    (expect {:opt "value"}
                            (schema-opts [:map {:opt "value"}])))))

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

(defn field-schema-field-id [Field]
  (assert-field-schema Field)
  ( -> (schema-opts Field) :id))

(defn field-schema-label [Field]
  (assert-field-schema Field)
  ( -> (schema-opts Field) :label))

;;;
;;; Doc
;;;

(defn init-doc-schema [Doc]
  (let [col-id (-> Doc schema-opts :firestore/collection)]
    (when-not col-id
      (js/console.error "Missing :firestore/collection in" Doc))
    (when-not (= "singletons" col-id)
      (reg-def :doc-schema (get Doc :doc-schema/id) Doc))
    (firestore/reg-doc-schema col-id Doc))
  Doc)

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

(defn doc-schema-router-param [Doc]
  (assert-doc-schema Doc)
  (str ":"
       (-> Doc schema-opts :doc-schema/symbol (str "Id"))))

(defn doc-schema-page-param [Doc]
  (assert-doc-schema Doc)
  (-> Doc schema-opts :doc-schema/symbol (str "-id") csk/->kebab-case-keyword))

(defn doc-schema-context-key [Doc]
  (assert-doc-schema Doc)
  (-> Doc schema-opts :doc-schema/symbol csk/->kebab-case-keyword))

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

(defn assert-subdoc-schema [Subdoc]
  ;; FIXME dev only
  (when-not (subdoc-schema? Subdoc)
    (throw (ex-info "subdoc schema expected"
                    {:value Subdoc}))))

(defn subdoc-schema-router-param [Subdoc]
  (assert-subdoc-schema Subdoc)
  (str ":"
       (-> Subdoc schema-opts :subdoc-schema/symbol (str "Id"))))

(defn subdoc-schema-id-generator [Subdoc]
  (or (-> Subdoc schema-opts :spark/id-generator)
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
       (get thing (keyword "cmd" "id"))))

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
;;; query
;;;

(defn query? [thing]
  (and (map? thing)
       (get thing (keyword "query" "id"))))

(defn assert-query [query]
  ;; FIXME dev only
  (when-not (query? query)
    (throw (ex-info "qeury expected"
                    {:value query}))))

(defn query-process [query]
  (assert-query query)
  (-> query
      :process))

;;;
;;; page
;;;

(defn page? [thing]
  (and (map? thing)
       (get thing (keyword "page" "id"))))

(defn assert-page [Page]
  ;; FIXME dev only
  (when-not (page? Page)
    (throw (ex-info "page expected"
                    {:value Page}))))

(defn- coerce-page-path [thing]
  (if (string? thing)

    thing

    (->> thing
         (map (fn [path-element]
                (cond
                  (keyword? path-element)
                  (str ":" (-> path-element csk/->camelCaseString))

                  (doc-schema? path-element)
                  (-> path-element doc-schema-router-param)

                  (subdoc-schema? path-element)
                  (-> path-element subdoc-schema-router-param)

                  :else
                  (str path-element))))
         (str/join "/")
         (str "/ui/"))))

(comment
  (coerce-page-path "/ui/book/:book")
  (coerce-page-path ["book" :book-id])
  (coerce-page-path []))

(defn page-path [Page]
  (-> Page :path coerce-page-path))

(defn page-docs [Page]
  (reduce (fn [docs path-element]
            (if (doc-schema? path-element)
              (assoc docs
                     (doc-schema-context-key path-element)
                     path-element)
              docs))
          {} (-> Page :path)))


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


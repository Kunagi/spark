(ns spark.firestore
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [goog.object :as gobj]
   [promesa.core :as p]
   [spark.env-config :as env-config]
   [spark.logging :refer [log]]
   [spark.utils :as u]))

;; https://firebase.google.com/docs/reference/js/firebase.firestore

(defn new-id []
  (str (random-uuid))
  ;; (u/nano-id)
  )

(defn doc? [doc]
  (and
   (map? doc)
   (-> doc :firestore/path string?)
   (-> doc :firestore/id string?)))

(s/def ::doc doc?)
(s/def ::path-element (s/or :map-path-element map?
                            :string-path-element string?))
(s/def ::path (s/or :keyword simple-keyword?
                    :string string?
                    :vector (s/coll-of ::path-element)
                    :doc ::doc))
(s/def ::opt-path (s/or :nil-path nil?
                        :non-nil-path ::path))

;; FIXME deprecated
(defn ^js firestore []
  (if-let [firestore (env-config/get! :firestore)]
    firestore
    (throw (js/Error. "FIRESTORE atom not initialized!"))))

;;; * helpers

(defn- ->firestore-json [o]
  (when o
    (js/JSON.stringify
     o (fn [k v]
         (cond
           (str/starts-with? k "__") js/undefined
           :else v)))))

(defn coerce-value [v]
  (when v
    (-> v clj->js u/->json js/JSON.parse ->firestore-json js/JSON.parse)))

(defn ^js FieldValue []
  (if (exists? js/firebase)
    (-> js/firebase.firestore.FieldValue)
    (-> (js/require "firebase-admin/firestore") .-FieldValue)))

(defn ^js array-remove [elements]
  (-> ^js (FieldValue) .-arrayRemove (apply (clj->js elements))))

(defn ^js array-union [elements]
  (-> ^js (FieldValue) .-arrayUnion (apply (clj->js elements))))

(defn ^js timestamp []
  (-> ^js (FieldValue) .serverTimestamp))

(declare inject-FieldValues)

(defn convert-FieldValue-or-nil [v]
  (when-let [k (cond
                 (vector? v)
                 (first v)

                 (and (qualified-keyword? v)
                      (-> v namespace (= "db")))
                 v
                 :else nil)]
    (cond
      (= k :db/set-value) (let [new-value (second v)
                                new-value (if (map? new-value)
                                            (inject-FieldValues new-value)
                                            new-value)]
                            new-value)
      (= k :db/array-union)  (let [elements (second v)
                                   elements (mapv (fn [v]
                                                    (cond
                                                      (map? v) (inject-FieldValues v)
                                                      (= :db/timestamp v) (timestamp)
                                                      :else v))
                                                  elements)]
                               (array-union elements))
      (= k :db/array-remove) (array-remove (second v))
      (= k :db/timestamp)    (timestamp)
      (= k :db/delete)       (-> ^js (FieldValue) .delete)
      (= k :db/increment)    (-> ^js (FieldValue) .-increment (apply [(if (vector? v)
                                                                        (second v)
                                                                        1)]))
      :else                  nil)))

(comment
  (convert-FieldValue-or-nil [:db/array-union [{:ts [:db/timestamp]}]]))

(defn inject-FieldValues [data]
  (reduce (fn [data [k v]]
            (if-let [converted-v (convert-FieldValue-or-nil v)]
              (assoc data k converted-v)
              (if (map? v)
                (assoc data k (inject-FieldValues v))
                data)))
          data data))

(defn remove-metadata [data]
  ;; (log ::remove-metadata
  ;;      :data data)
  (let [data (dissoc data
                     :firestore/id
                     :firestore/schema
                     :firestore/doc-path
                     :firestore/path
                     :firestore/create
                     :firestore/exists?
                     :db/type
                     :db/id
                     :db/ref
                     :db/exists)]
    (reduce (fn [data [k v]]
              (if (map? v)
                (assoc data k (remove-metadata v))
                data))
            data data)))

(defn encode-field-key [k]
  (cond
    (qualified-keyword? k) (str (namespace k) ":" (name k))
    (simple-keyword? k)    (name k)
    :else                  k))

(defn decode-field-key [s]
  (let [parts (str/split s ":")]
    (if (-> parts count (> 1))
      (keyword (first parts) (->> parts rest (str/join ":")))
      (keyword (first parts)))))

(defn- entity--encode-keys [data]
  (reduce (fn [data [k v]]
            (let [new-k (encode-field-key k)
                  new-v (if (map? v)
                          (entity--encode-keys v)
                          v)]
              ;; (log ::DEBUG
              ;;      :k k :new-k new-k)
              (assoc data new-k new-v)))
          data data))

;; * conform to schema

(defn conform-js-data [^js data schema db-doc-ref]
  (let [schema-type (if (vector? schema)
                      (first schema)
                      schema)]
    ;; (when (= :keyword schema)
    ;;   (js/console.log "!!!" "keyword schema:" data (keyword data)))
    (cond

      (nil? data)
      nil

      (= :keyword schema-type)
      (keyword data)

      (= :string schema-type)
      (str data)

      (= :int schema-type)
      (js/parseInt data)

      ;; FIXME ???
      (or (string? data) (number? data) (boolean? data))
      (js->clj data)

      (instance? (if (exists? js/firebase)
                   (-> js/firebase.firestore.Timestamp)
                   (-> (js/require "firebase-admin/firestore") .-Timestamp))
                 data)
      (-> data .toDate)

      ^boolean (js/Array.isArray data)
      (case schema-type
        :set
        (into #{} (map (fn [[idx item]]
                         (let [item-schema (if (map? (second schema))
                                             (nth schema 2)
                                             (nth schema 1))]
                           (conform-js-data item item-schema (conj db-doc-ref idx))))
                       (map-indexed vector data)))

        :vector
        (mapv (fn [[idx item]]
                (let [item-schema (if (map? (second schema))
                                    (nth schema 2)
                                    (nth schema 1))]
                  (conform-js-data item item-schema (conj db-doc-ref idx))))
              (map-indexed vector data))

        ;; else
        (mapv (fn [[idx item]]
                (conform-js-data item nil (conj db-doc-ref idx)))
              (map-indexed vector data)))

      (vector? schema)
      (case schema-type

        :map-of
        (reduce (fn [m js-key]
                  (let [k           js-key
                        v           (gobj/get data js-key)
                        k-schema    (if (map? (second schema))
                                      (nth schema 2)
                                      (nth schema 1))
                        v-schema    (if (map? (second schema))
                                      (nth schema 3)
                                      (nth schema 2))
                        k-conformed (conform-js-data k k-schema db-doc-ref)]
                    (if (nil? v)
                      m
                      (assoc m
                             k-conformed
                             (conform-js-data v v-schema (conj db-doc-ref
                                                               k-conformed))))))
                {} (js/Object.keys data))

        ;; else -> :map
        (let [new-data {:db/ref db-doc-ref}
              new-data (if-let [doc-schema-id (or (-> schema second :subdoc-schema/id)
                                                  (-> schema second :doc-schema/id))]
                         (assoc new-data :firestore/schema doc-schema-id)
                         new-data)]
          (reduce (fn [m js-key]
                    (let [k        (decode-field-key js-key)
                          v        (gobj/get data js-key)
                          v-schema (u/malli-map-field-schema-by-id schema k)
                          v        (conform-js-data v v-schema (conj db-doc-ref k))]
                      (if (nil? v)
                        (assoc m k nil)
                        (assoc m k v))))
                  new-data (js/Object.keys data))))

      :else
      (js->clj data :keywordize-keys true))))

;; * wrap docs to have access to id and path

(defonce DOC_SCHEMAS (atom {}))

(defn reg-doc-schema [col-id doc-schema]
  (swap! DOC_SCHEMAS assoc col-id doc-schema))

(defn conform-doc-data [^js data schema db-doc-ref]
  (when data
    (-> data
        (conform-js-data schema db-doc-ref)
        (assoc :firestore/schema (-> schema second :doc-schema/id)))))

(defn doc-schema [col-id]
  (get @DOC_SCHEMAS col-id))

(defn wrap-doc [^js query-doc-snapshot]
  (let [firestore-data (-> query-doc-snapshot .data)
        firestore-ref (-> query-doc-snapshot .-ref)
        path (-> firestore-ref .-path)
        col-id (.substring path 0 (.indexOf path "/"))
        doc-schema (doc-schema col-id)
        db-doc-ref [path]]
    (-> firestore-data
        (conform-doc-data doc-schema db-doc-ref)
        (assoc :firestore/id (-> query-doc-snapshot .-id)
               :firestore/path path
               :firestore/exists? (boolean firestore-data)
               :firestore/ref firestore-ref
               ;; :firestore/data firestore-data
               :db/id (-> query-doc-snapshot .-id)
               :db/exists (boolean firestore-data)
               :db/ref path
               :db/type col-id))))

(defn wrap-docs [^js query-snapshot]
  (mapv wrap-doc (-> query-snapshot .-docs)))

(defn ^js unwrap-doc [doc]
  ;; (log ::unwrap-doc
  ;;      :doc doc)
  (when doc
    (-> doc
        (dissoc :firestore/id
                :firestore/path
                :firestore/exists?
                :firestore/schema
                :firestore/create
                :firestore/ref
                :firestore/data
                :db/id
                :db/exists
                :db/ref)
        inject-FieldValues
        remove-metadata
        entity--encode-keys
        clj->js)))

(defn doc-id [doc]
  (-> doc :db/id))

(defn doc-path [doc]
  (-> doc :db/ref))

(defn doc-exists? [doc]
  (-> doc :db/exists boolean))



;;; collection and doc references

(defn as-path [thing]
  (cond
    (string? thing)  (-> thing (.split "/"))
    (doc? thing)     (-> thing doc-path as-path)
    (keyword? thing) [(name thing)]
    :else            (do (s/assert ::path thing) thing)))

;; (declare ref)

(defn- fs-collection [source path-elem]
  (if (map? path-elem)
    (let [{:keys [id wheres where order-by limit start-after start-at end-at]} path-elem
          wheres                    (if where
                                      (conj wheres where)
                                      wheres)
          wheres                    (remove nil? wheres)
          collection                (-> ^js source (.collection id))
          collection (reduce (fn [collection [attr op val]]
                               (-> ^js collection (.where attr op (clj->js val))))
                             collection wheres)
          collection (if-not order-by
                       collection
                       (-> ^js collection (.orderBy (first order-by) (second order-by))))
          ;; collection (if-not start-after
          ;;              collection
          ;;              (-> ^js collection (.startAfter (ref start-after))))
          collection (if-not start-at
                       collection
                       (-> ^js collection (.startAt start-at)))
          collection (if-not end-at
                       collection
                       (-> ^js collection (.endAt end-at)))
          collection (if-not limit
                       collection
                       (-> ^js collection (.limit limit)))]
      collection)
    (-> ^js source (.collection path-elem))))

(defn ^js ref [thing]
  ;; (log ::ref
  ;;      :path path)
  (s/assert ::opt-path thing)
  (when thing
    (or

     (when (map? thing)
       (-> thing :firestore/ref))

     (loop [col  nil
            doc  nil
            path (as-path thing)]
       (if (empty? path)
         (if doc doc col)
         (cond

           doc
           (recur (-> ^js doc (fs-collection (first path)))
                  nil
                  (rest path))

           col
           (recur nil
                  (-> ^js col (.doc (first path)))
                  (rest path))

           :else
           (recur (-> (firestore) (fs-collection (first path)))
                  nil
                  (rest path))))))))


(defn doc-ref [thing]
  (ref thing))

(defn col-ref [thing]
  (ref thing))

;;;

(defn doc> [path]
  (js/Promise.
   (fn [resolve reject]
     (-> (doc-ref path)
         .get
         (.then (fn [^js doc]
                  (resolve (wrap-doc doc)))
                reject)))))

(defn col> [path]
  ;; (log ::col>
  ;;      :path path)
  (js/Promise.
   (fn [resolve reject]
     (-> (col-ref path)
         .get
         (.then (fn [^js query-snapshot]
                  (resolve (wrap-docs query-snapshot)))
                reject)))))

(defn cols-names> []
  (u/=> (-> (firestore) .listCollections)
        #(mapv (fn [^js col] (.-id col)) %)))

(defn create-doc>
  "Creates a new document.
  An existing document will be replaced."
  [path data]
  (log ::create-doc>
       :value path
       :data data)
  (s/assert ::path path)
  (let [^js ref  (doc-ref path)
        col-ref? (-> ref .-where boolean)]
    (if col-ref?
      (-> ref (.add (unwrap-doc data)))
      (-> ref (.set (unwrap-doc data))))))

(defn save-doc>
  "Saves the document `doc`."
  [doc]
  (s/assert doc? doc)
  (-> doc
      doc-path
      doc-ref
      (.set ^js (unwrap-doc doc))))

(defn update-fields>
  "Updates fields in an existing document."
  [doc-path fields]
  (log ::update-fields>
       :value doc-path
       :fields fields)
  (s/assert ::path doc-path)
  (s/assert map? fields)
  (-> doc-path
      doc-ref
      (.update (unwrap-doc fields))))

(defn- flatten-entity-map
  ([m]
   (flatten-entity-map nil nil m))
  ([doc prefix m]
   ;; (log ::flatten-entity-map
   ;;      :m m)
   (let [m (remove-metadata m)]
     (reduce (fn [doc [k v]]
               (let [k (encode-field-key k)
                     k (if prefix
                         (str prefix "." k)
                         k)]
                 (if (map? v)
                   (flatten-entity-map doc k v)
                   (assoc doc k v))))
             doc m))))

(comment
  (flatten-entity-map {:skills {"java" "good"}
                       :ts-updated (timestamp)})
  (flatten-entity-map {:id     "1"
                       :name   "witek"
                       :skills {"java"    {:id   "java"
                                           :name "Java"}
                                "clojure" {:id   "clojure"
                                           :name "Clojure"}}}))

(defn update-child-fields> [doc child-path child-id child-changes]
  (let [changes (flatten-entity-map {} (str child-path "." child-id) child-changes)]
    (update-fields> doc changes)))

(defn delete-doc>
  [doc-or-path]
  (log ::delete-doc
       :value doc-or-path)
  (-> doc-or-path doc-ref .delete))

(defn load-and-save>
  "Load, update and save/delete.
  Deletes the document if `update-f` returns nil."
  [doc-path update-f]
  (log ::load-and-save>
       :value doc-path
       :update-f update-f)
  (s/assert ::path doc-path)
  (-> (doc> doc-path)
      (.then #(if (doc-exists? %)
                (let [doc (update-f %)]
                  (cond
                    (= doc :db/delete) (delete-doc> %)
                    doc                (save-doc> doc)
                    :else              (js/Promise.resolve :db/no-op)))
                (let [data (update-f nil)]
                  (cond
                    (= :db/delete data)
                    (delete-doc> doc-path)

                    (seq data)
                    (create-doc> doc-path data)

                    :else
                    (js/Promise.resolve nil)))))))

;;; transactions

(defn get>
  ([path]
   (if path
     (get> nil path nil)
     (u/no-op>)))

  ([transaction path]
   (get> transaction path nil))

  ([^js transaction path not-found]
   (log ::get>
        :value        path
        :transaction (boolean transaction))
   (let [ref      (ref path)
         col-ref? (-> ref .-where boolean)]
     (u/=> (if transaction
             (.get transaction ref)
             (.get ref))
           (fn [^js result]
             ;; (log ::get>--2
             ;;      :path path
             ;;      :transaction transaction)
             (let [ret (if col-ref?
                         (wrap-docs result)
                         (if (-> result .-exists)
                           (wrap-doc result)
                           not-found))]
               ;; (log ::get>--3
               ;;      :path path
               ;;      :transaction transaction)
               ret))))))

(def get-doc> get>)
(def get-col> get>)

(comment
  (u/=> (get-doc> ["devtest" "dummy-1"]) u/tap>)
  (u/=> (get-doc> "devtest/dummy-1") u/tap>)
  (u/=> (get-col> ["devtest"]) u/tap>))

(defn- set>--set-doc> [^js transaction tx-data autocreate?]
  ;; (log ::set>--set-doc>
  ;;      :tx-data tx-data)
  (let [ref  (or (-> tx-data :firestore/ref)
                 (doc-ref (or (-> tx-data :firestore/path)
                              (-> tx-data :db/ref))))
        create? (-> tx-data :firestore/create)
        tx-data (if create?
                  tx-data
                  (flatten-entity-map tx-data))
        js-data (unwrap-doc tx-data)

        result (if autocreate?
                 (if transaction
                   (if create?
                     (.set transaction ref js-data)
                     (.update transaction ref js-data))
                   (-> (.update ref js-data)
                       (.catch (fn [_err]
                                 (.set ref js-data (clj->js {:merge true}))))))
                 (if transaction
                   (u/resolve> (.update transaction ref js-data))
                   (.update ref js-data)))]
    (if (u/promise? result)
      (-> result
          (.then (fn [_ok] tx-data)
                 (fn [error]
                   (throw (ex-info (str "Error while set>--set-doc> for " ref
                                        " | " error)
                                   {:error error
                                    :tx-data tx-data}
                                   error)))))
      (u/resolve> result))))

(defn- set>--delete-doc> [^js transaction tx-data]
  (let [ref (or (-> tx-data :firestore/ref)
                (doc-ref (or (-> tx-data :firestore/path)
                             (-> tx-data :db/ref))))]
    (u/=> (if transaction
            (u/resolve> (.delete transaction ref))
            (.delete ref))
          (fn [_] tx-data))))

(defn subdoc-prefix-from-db-ref [db-ref]
  (->> db-ref
       rest
       (map (fn [path-element]
              (if (keyword? path-element)
                (name path-element)
                (str path-element))))
       (str/join ".")))

(defn set>--delete-subdoc> [^js transaction tx-data db-ref]
  (set>--set-doc> transaction
                  {:firestore/path                    (first db-ref)
                   (subdoc-prefix-from-db-ref db-ref) [:db/delete]}
                  false))

(defn set>--set-subdoc> [^js transaction tx-data db-ref]
  (let [doc-path (first db-ref)
        entity   (flatten-entity-map {:firestore/path doc-path}
                                     (subdoc-prefix-from-db-ref db-ref)
                                     tx-data)]
    (set>--set-doc> transaction entity false)))

(declare transact>)

(defn set>
  ([tx-data]
   (set> nil tx-data))
  ([^js transaction tx-data]
   ;; (log ::set>
   ;;      :tx-data tx-data)
   (cond

     (nil? tx-data)
     (u/no-op>)

     ;; sequential tx-data
     (sequential? tx-data)
     (if (empty? tx-data)
       (u/no-op>)
       (if (-> tx-data count (> 500))
         (p/let [[batch rest-batch] (split-at 500 tx-data)
                 batch-results (set> transaction batch)
                 rest-results (set> transaction rest-batch)]
           [batch-results rest-results])
         (if transaction
           (u/all-in-sequence> (map #(set> transaction %) tx-data))
           (transact> (fn [{:keys [set>]}]
                        (u/all-in-sequence> (map #(set> %) tx-data)))))))

     ;; not sequential tx-data (just entity)
     :else
     (let [db-ref  (-> tx-data :db/ref)
           subdoc? (vector? db-ref)]
       (log ::set>
            :tx-data tx-data
            :transaction (boolean transaction))
       (if subdoc?
         (if (-> tx-data :db/delete (= true))
           (set>--delete-subdoc> transaction tx-data db-ref)
           (set>--set-subdoc> transaction tx-data db-ref))
         (if (-> tx-data :db/delete (= true))
           (set>--delete-doc> transaction tx-data)
           (set>--set-doc> transaction tx-data true)))))))

(comment

  (u/tap> (set> [{:firestore/path "devtest/deleteme" :delete "me1"}]))

  (u/tap> (set> {:firestore/path "devtest/array"
                 :array [:db/array-union [{:ts [:db/timestamp]}]]}))

  (set> nil)
  (set> {:firestore/path "devtest/dummy-1" :hello "world"})
  (u/tap>
   (set> [{:firestore/path "devtest/dummy-1" :set-1 [:db/timestamp]}
          {:firestore/path "devtest/dummy-2" :set-2 [:db/timestamp]}]))
  (set> {:firestore/path "devtest/dummy-1" :hello [:db/delete]})
  (u/tap> (set> {:firestore/path "devtest/dummy-1" :db/delete true}))
  (u/tap> (get-doc> "devtest/dummy-1"))

  (u/=> (get-doc> "devtest/dummy-1")
        (fn [doc]
          (js/console.log "LOADED" doc)
          (set> [{:db/ref   "devtest/dummy-1"
                  :ts       [:db/timestamp]
                  :children {"a" {:name "a"}
                             "b" {:name "b"}}}]))
        (fn [result]
          (js/console.log "RESULT" result))
        u/tap>)

  (u/tap> (set> {:db/ref    ["devtest/dummy-1" :children "b"]
                 :db/delete true}))

  (u/=> (get-doc> "devtest/dummy-1")
        (fn [doc]
          (js/console.log "LOADED" doc)
          (set> [{:db/ref ["devtest/dummy-1" :children "c"]
                  :id     "c"
                  :ts     [:db/timestamp]}]))
        (fn [result]
          (js/console.log "RESULT" result))
        u/tap>))

;; https://firebase.google.com/docs/reference/js/v8/firebase.firestore.Firestore#runtransaction
(defn transact>
  ([transaction>]
   (transact> nil transaction>))
  ([message transaction>]
   (log ::transact>
        :value message
        :tx-data (str transaction>))
   (let [starttime (js/Date.)]
     (if (fn? transaction>)

       ;; transaction function
       (-> (firestore)
           (.runTransaction
            (fn [^js transaction]
              (log ::transact>--2
                   :message message
                   :runtime (- (-> (js/Date.) .getTime) (-> starttime .getTime)))
              (u/as>
               (p/let [result (transaction> {:get> (partial get> transaction)
                                             :get-doc> (partial get-doc> transaction)
                                             :get-col> (partial get-col> transaction)
                                             :set> (partial set> transaction)})]
                 (log ::transact>--fn-completed
                      :message message
                      :result result)
                 result))))
           (.then identity
                  (fn [error]
                    (throw (ex-info (str "Error in transaction '"
                                         message
                                         "' | " error)
                                    {:transaction transaction>}
                                    error)))))

       ;; transaction data
       (set> transaction>)))))

(comment

  (u/tap>
   (transact> (fn [{:keys [get> set>]}]
                (u/=> (get> ["devtest" "dummy-1"])
                      (fn [dummy-1]
                        (set> [{:firestore/path (-> dummy-1 :firestore/path)
                                :ts [:db/timestamp]}]))))))

  (transact> {:db/ref "devtest/dummy-2" :hello "2nd world"})
  (transact> {:db/ref "devtest/dummy-2" :db/delete true})
  (def id "dummy-3")
  (let [transaction (fn [{:keys [get> set>]}]
                      (u/=> (get> ["devtest" id])
                            (fn [dummy]
                              (js/console.log "DEBUG dummy-loaded" dummy)
                              (set> [{:firestore/path (str "devtest/" id)
                                      :counter        (inc (-> dummy :counter))}]))))]
    (u/=> (transact> transaction)
          u/tap>)))

(comment
  (u/tap> (transact> (fn [{:keys [set>]}]
                       (set> {:db/ref "devtest/new"
                              :hello  :world})))))

(defn delete-docs> [path]
  (p/let [docs (get-col> path)]
    (transact>
     (mapv (fn [doc]
             {:firestore/path (-> doc :firestore/path)
              :db/delete      true})
           docs))))

(comment
  (p/let [doc-1 (get-doc> "devtest/dummy-1")
          doc-2 (get-doc> "devtest/dummy-2")]
    (u/tap> {:doc-1 doc-1
             :doc-2 doc-2})))

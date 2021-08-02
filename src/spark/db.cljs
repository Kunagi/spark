(ns spark.db
  (:require
   [clojure.string :as str]

   [spark.utils :as u]
   [spark.core :as spark]
   [spark.firestore :as firestore]))

;; * common

(defn- entity-type->ref [entity-type id]
  (when-not (or (string? entity-type)
                (keyword? entity-type)
                (spark/doc-schema? entity-type))
    (throw (ex-info (str "add-tx: entity-type must be keyword, string or doc-schema")
                    {:entity-type entity-type
                     :id          id})))
  (str (cond
         (keyword? entity-type)
         (name entity-type)

         (spark/doc-schema? entity-type)
         (spark/doc-schema-col-path entity-type)

         :else
         entity-type)
       "/" id))

;; * read

(defn get>
  ([path]
   (firestore/get> path))
  ([entity-type id]
   (get> (entity-type->ref entity-type id))))

(comment
  (u/tap> (get> "devtest" "dummy-1"))
  (u/tap> (get> :devtest "dummy-1"))
  (u/tap> (get> "devtest"))
  (u/tap> (get> :devtest))
  )

;; * write

(def set> firestore/set>)

(defn- entity--update-id [entity]
  (if (-> entity :id)
    entity
    (let [ref (-> entity :db/ref)
          id  (or (if (string? ref)
                    (second (str/split ref "/"))
                    (last ref))
                  (str (random-uuid)))]
      (assoc entity :id id))))

(comment
  (entity--update-id {})
  (entity--update-id {:db/ref "devtest/dummy-1"})
  (entity--update-id {:db/ref ["devtest/dummy-1" :children "child-1"]})
  (str/split "hallo/welt" "/"))

(defn- entity--assert-ref [entity]
  (let [ref (-> entity :db/ref)]
    (when-not ref
      (throw (ex-info "Enity is missing :db/ref"
                      {:entity entity})))
    (cond

      (string? ref)
      (when-not (= 2 (count (str/split ref "/")))
        (throw (ex-info (str "Invalid entity :db/ref: '" ref "'")
                        {:ref    ref
                         :entity entity})))

      (vector? ref)
      (when (or (< (count ref) 3)
                (even? (count ref)))
        (throw (ex-info (str "Invalid entity :db/ref: '" ref "'")
                        {:ref    ref
                         :entity entity})))

      :else
      (throw (ex-info (str "Invalid entity :db/ref: '" ref "'")
                      {:ref    ref
                       :entity entity}))))
  entity)

(defn- entity--migrate-path-to-ref [entity]
  (if (-> entity :db/ref)
    entity
    (if-let [path (-> entity :firestore/path)]
      (assoc entity :db/ref
             (if (string? path)
               path
               (->> path
                    (str/join "/")))
             )
      entity)))

(defn conform-tx-data-entity [entity]
  (when (seq (dissoc entity :db/ref))
    (-> entity
        entity--migrate-path-to-ref
        entity--assert-ref
        entity--update-id
        (assoc :ts-updated [:db/timestamp])
        )))

(comment
  (conform-tx-data-entity {})
  (conform-tx-data-entity {:db/ref "devtest/dummy-1"})
  (conform-tx-data-entity {:db/ref "devtest/dummy-1" :name "dummy"})
  (conform-tx-data-entity {:db/ref ["devtest/dummy-1" :children "child-1"]})
  (conform-tx-data-entity {:db/ref ["devtest/dummy-1" :children "child-1"]
                           :name   "dummy"})
  (conform-tx-data-entity {:firestore/path ["devtest" "summy-1"]})
  (str/split "hallo/welt" "/"))

(defn conform-tx-data [tx-data]
  (when tx-data
    (if (map? tx-data)
      (conform-tx-data-entity tx-data)
      (->> tx-data
           (map conform-tx-data-entity)
           (remove nil?)))))


(defn transact> [tx-data]
  (firestore/transact> (if (fn? tx-data)
                         tx-data
                         (conform-tx-data tx-data))))


(defn ->ref [thing]
  (when thing
    (cond
      (string? thing) thing
      (vector? thing) thing
      (map? thing)    (or (-> thing :db/ref)
                          (-> thing :firestore/path))
      :else           (throw (ex-info (str "Invalid ref object type")
                                      {:type   (type thing)
                                       :object thing})))))

(comment
  (->ref nil)
  (->ref "devtest/dummy")
  (->ref {:db/ref "devtest/dummy"})
  (->ref {:db/ref ["devtest/dummy" :children "child-1"]})
  )


(defn add-tx [entity-type values]
  (let [id  (or (-> values :id)
                (str (random-uuid)))
        ref (entity-type->ref entity-type id)]
    (assoc values
           :id id
           :db/ref ref)))

(defn add> [entity-type values]
  (transact> (add-tx entity-type values)))

(defn update-tx [thing values]
  (when (seq values)
    (assoc values :db/ref (->ref thing))))

(comment
  (update-tx {:db/ref "some/entity"} {:change "this"}))

(defn update> [thing values]
  (transact> (update-tx thing values)))

(defn delete-tx [thing]
  (when thing
    {:db/ref    (->ref thing)
     :db/delete true}))

(defn delete> [thing]
  (transact> (delete-tx thing)))

(defn add-child-tx [parent-thing path values]
  ;; (when-not (vector? path)
  ;;   (throw (ex-info "add-child-tx: path must be a vector"
  ;;                   {:thing thing
  ;;                    :path  path
  ;;                    :values  values})))
  (let [id  (or (-> values :id)
                (str (random-uuid)))
        ref (let [ref (->ref parent-thing)]
              (-> (if (string? ref) [ref] ref)
                  (into (if (vector? path) path [path]))
                  (conj id)))]
    (assoc values
           :db/ref ref
           :id id)))

(comment
  (add-child-tx "devtest/db-1" :children {:some "child"})
  (add-child-tx ["devtest/db-1" :group "g1"] :children {:some "child"}))

(defn add-child> [parent-thing path values]
  (transact> (add-child-tx parent-thing path values)))


(comment
  (js/console.log "smoketest")

  (add> "devtest" {})

  (update> "devtest/db-1" {})
  (update> "devtest/db-1" {:name "witek"})
  (delete> "devtest/db-1")

  (update> {:db/ref "devtest/db-1"} {:name "witek"})
  (delete> {:db/ref "devtest/db-1"})

  (update> {:firestore/path "devtest/db-1"} {:name "witek"})

  (update> {:db/ref ["devtest/db-1" :friends "kacper"]} {:name "Kacper"})
  (delete> {:db/ref ["devtest/db-1" :friends "kacper"]})

  (add-child> "devtest/db-1" :group {:name "Group #n"})
  (add-child> "devtest/db-1" :group {:id   "g2"
                                     :name "Group #2"})

  (add-child> {:db/ref ["devtest/db-1" :group "g2"]} :people {:name "Olga"})
  )

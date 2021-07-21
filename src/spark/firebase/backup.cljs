(ns spark.firebase.backup
  (:require
   [tick.locale-en-us]
   [tick.timezone]
   [tick.alpha.api :as tick]
   [tick.format :as tick.format]
   ["firebase-admin" :as admin]

   [spark.logging :refer [log]]
   [spark.firestore :as firestore]
   [spark.utils :as u]
   [spark.gcf :as gcf]))

;; https://firebase.google.com/docs/storage/admin/start
;; https://firebase.google.com/docs/reference/admin/node/admin.storage

;; https://googleapis.dev/nodejs/storage/latest/Bucket.html
(defn bucket [bucket-name]
  (-> ^js admin .storage (.bucket (str "gs://" bucket-name))))

(defn file [bucket path]
  (-> ^js bucket (.file path)))

(comment
  (u/=> (firestore/col> [ "radars"])
        (fn [result]
          (js/console.log "DEBUG-1" result))))

(defn write-string> [bucket path value]
  (u/promise>
   (fn [resolve _reject]
     (let [f        (file bucket path)
           writable (-> ^js f .createWriteStream)]
       (-> writable ^js (.on "finish" #(resolve path)))
       (-> writable ^js (.write value))
       (-> writable ^js .end)
       ))))

(comment
  (-> (bucket "happygast-backup") .-acl .get)
  (u/=> (write-string> (bucket "happygast-backup") "dummy.txt" "42")
        u/tap>))

(defn download-string [bucket path]
  (let [file (file bucket path)]
    (-> ^js file .download)))

(defn write-doc> [bucket path doc]
  (let [path (str path "/" (-> doc :firestore/path) ".edn")
        s    (u/->edn doc)]
    (write-string> bucket path s)))

(defn- write-next-doc> [bucket path docs results]
  (if-let [doc (first docs)]
    (u/=> (write-doc> bucket path doc)
          (fn [result]
            (write-next-doc> bucket path (rest docs) (conj results result))))
    (u/as> results))
  )

(defn write-col> [bucket path col-name docs]
  (let [path (str path "/" col-name ".edn")
        colmap (reduce (fn [m doc]
                         (assoc m (:id doc) doc))
                       {} docs)
        s    (u/->edn colmap)]
    (write-string> bucket path s)))

(defn backup-col> [bucket path col-name]
  (log ::backup-col>
       :bucket bucket
       :path path
       :col col-name)
  (u/=> (firestore/col> [col-name])
        (fn [docs]
          (write-col> bucket path col-name docs)
          #_(write-next-doc> bucket path docs [])
          #_(u/all> (map (partial write-doc> bucket path) docs))
          )))

(defn- backup-next-col> [bucket path cols-names results]
  (if-let [col-name (first cols-names)]
    (u/=> (backup-col> bucket path col-name)
          (fn [result]
            (backup-next-col> bucket path (rest cols-names) (conj results result))))
    (u/as> results)))

(defn backup-cols> [bucket path cols-names]
  (backup-next-col> bucket path cols-names [])
  #_(u/all>
     (map #(backup-col> bucket path %)
          cols-names))
  )

(def date-path-format (tick.format/formatter "yyyy/MM/dd"))

(defn date-path []
  (let [now  (tick/now)
        date (tick/date now)]
    (str
     (tick/format date-path-format date)
     "/"
     (tick/now))))

(defn backup-all> [bucket-name]
  (log ::backup-all>
       :bucket bucket-name)
  (let [bucket (bucket bucket-name)
        path   (date-path)]
    (u/=> (firestore/cols-names>)
          (fn [cols-names]
            (backup-cols> bucket path cols-names)))))

(comment
  (-> (backup-all> "happygast-backup")
      u/tap>))


(defn handle-on-backup> [bucket-name ^js _req]
  (backup-all> bucket-name))

(defn exports [bucket-name]
  {

   :backup
   (gcf/on-request--format-output> (partial handle-on-backup> bucket-name))

   })

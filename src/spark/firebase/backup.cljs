(ns spark.firebase.backup
  (:require
   [clojure.string :as str]
   [spark.time :as time]
   [promesa.core :as p]
   ["firebase-admin" :as admin]
   ["stream" :as stream]

   [spark.logging :refer [log]]
   [spark.firestore :as firestore]
   [spark.db :as db]
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
  (u/=> (firestore/col> ["radars"])
        (fn [result]
          (js/console.log "DEBUG-1" result))))

(defn write-string> [bucket path value]
  (u/promise>
   (fn [resolve _reject]
     (let [f        (file bucket path)
           writable (-> ^js f .createWriteStream)]
       (-> writable ^js (.on "finish" #(resolve path)))
       (-> writable ^js (.write value))
       (-> writable ^js .end)))))

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
    (u/as> results)))

(defn write-col> [bucket path col-name docs]
  (let [path (str path "/" col-name ".edn")
        colmap (reduce (fn [m doc]
                         (assoc m
                                (or (:id doc)
                                    (:db/id doc))
                                doc))
                       {} docs)
        s    (u/->edn colmap)]
    (write-string> bucket path s)))

(defn backup-col> [bucket path col-name]
  (log ::backup-col>
       :bucket bucket
       :path path
       :col col-name)
  (let [date-start (js/Date.)]
    (p/let [docs (firestore/col> [col-name])
            date-loaded (js/Date.)
            result (write-col> bucket path col-name docs)
            date-written (js/Date.)
            _ (log ::backup-col>--completed
                   :col col-name
                   :docs (count docs)
                   :runtime-load (- (-> date-loaded .getTime) (-> date-start .getTime))
                   :runtime-write (- (-> date-written .getTime) (-> date-loaded .getTime))
                   :runtime-total (- (-> date-written .getTime) (-> date-start .getTime)))]
      result)))

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
          cols-names)))

(def date-path-format (time/formatter "yyyy/MM/dd"))

(defn date-path []
  (let [now  (time/now)
        date (time/date now)
        time (time/time now)]
    (str
     (time/format date-path-format date)
     "/"
     time)))

;; * Single File

(defn stream-string> [^js output-stream s]
  (if (str/blank? s)
    (u/resolve> output-stream)
    (u/promise>
     (fn [resolve reject]
       #_(-> res (.write s "utf-8" resolve))
       (let [Readable (-> stream .-Readable)
             readable (-> Readable (.from s))]
         (-> readable (.pipe output-stream (clj->js {:end false})))
         (-> readable (.on "end" (fn []
                                   (resolve output-stream))))
         (-> readable (.on "error" (fn [err]
                                     (reject err)))))))))

(defn stream-col> [^js res col-name]
  (log ::stream-col-to-http-response>
       :col col-name)
  (p/let [start-millis (-> (js/Date.) .getTime)
          docs (db/get> [{:id col-name}])
          _ (log ::stream-col-to-http-response>--col-loaded
                 :col col-name
                 :runtime (-> (js/Date.) .getTime (- start-millis)))

          start-millis (-> (js/Date.) .getTime)
          docs (->> docs
                    (map db/doc-remove-metadata))
          data {:col col-name
                :docs (->> docs #_(take 2)) #_(count docs)} ;; FIXME
          ;; s (u/->edn data)
          s (pr-str data)
          _ (log ::stream-col-to-http-response>--ednized
                 :runtime (-> (js/Date.) .getTime (- start-millis)))

          start-millis (-> (js/Date.) .getTime)
          _ (stream-string> res s)
          _ (log ::stream-col-to-http-response>--streamed-to-file
                 :runtime (-> (js/Date.) .getTime (- start-millis)))
          ]
    res))

(defn stream-next-col> [^js output-stream cols-names]
  (when (seq cols-names)
    (p/let [_ (stream-col> output-stream (first cols-names))]
      (stream-next-col> output-stream (rest cols-names)))))

(defn create-backup-file> [bucket filename cols-names]
  (p/let [;; new-auth-token  (if goog.DEBUG "geheim"
          ;;                     (u/nano-id))
          ;; _ (db/update> "secrets/singleton" {:backup-auth-token new-auth-token})

          file (-> ^js bucket (.file filename))
          output-stream (-> ^js file .createWriteStream)

          _ (stream-string> output-stream (u/->edn {:ts (str (js/Date.))}))
          _ (stream-next-col> output-stream cols-names)
          ;; _ (u/all-in-sequence> (->> cols-names
          ;;                            (map #(stream-col-to-http-response> res %))))

          _ (-> output-stream .end)]
    {:filename filename}))

;; *

(defn backup-all-except> [bucket-name exceptions]
  (log ::backup-all-except>
       :bucket bucket-name
       :exceptions exceptions)
  (let [bucket (bucket bucket-name)
        path   (str "backup/" (date-path))
        filename (str path ".edn")]
    (p/let [cols-names (firestore/cols-names>)
            exceptions (into #{} exceptions)
            cols-names (->> cols-names
                            (remove #(contains? exceptions %)))
            _ (create-backup-file> bucket filename cols-names)]
      ;; (backup-cols> bucket path cols-names)
      filename)))

(defn backup-all> [bucket-name]
  (backup-all-except> bucket-name #{}))

(defn handle-on-backup> [bucket-name exceptions ^js _req]
  (backup-all-except> bucket-name exceptions))

(defn exports [bucket-name exceptions]
  {:backup (gcf/on-request> (partial handle-on-backup> bucket-name exceptions)
                            {:timeoutSeconds 540
                             :memory "8GB"})})

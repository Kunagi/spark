(ns spark.firebase.storage
  (:require
   ["firebase/storage" :as firebase-storage]
   [promesa.core :as p]
   [spark.env-config :as env-config]
   [spark.logging :refer [log]]
   [spark.utils :as u]))

;; https://firebase.google.com/docs/reference/js/firebase.storage.Reference

(defn- initialize []
  (log ::initialize)
  (let [firebase-app (env-config/get! :firebase-app)
        service (firebase-storage/getStorage firebase-app)]
    (when ^boolean goog.DEBUG
      (firebase-storage/connectStorageEmulator service "localhost" 9199))
    service))

(def default-storage (memoize initialize))

(defn storage [bucket-name]
  (firebase-storage/getStorage (env-config/get! :firebase-app)
                               (str "gs://" bucket-name)))

(defn storage-ref
  ([]
   (storage-ref nil))
  ([bucket-name]
   (if bucket-name
     (firebase-storage/ref (storage bucket-name))
     (firebase-storage/ref (default-storage)))))

(defn ref
  ([path]
   (ref nil path))
  ([bucket-name path]
   (cond
     (string? path)
     (firebase-storage/ref
      (if bucket-name
        (storage bucket-name)
        (default-storage))
      path)

     (vector? path)
     (reduce (fn [ref path-element]
               (when ref
                 (when path-element
                   (firebase-storage/ref ref (str path-element)))))
             (storage-ref bucket-name) path)

     :else
     path                               ; asuming it is already a ref
     #_(throw (ex-info "Unsupported path type"
                       {:path path
                        :type (type path)})))))

(defn url>
  ([path]
   (url> nil path))
  ([bucket-name path]
   (js/Promise.
    (fn [resolve _reject]
      (let [ref (ref bucket-name path)]
        (if ref
          (-> (firebase-storage/getDownloadURL ref)
              (.then resolve #(resolve nil)))
          (resolve nil)))))))

(defn list-files>
  ([path]
   (list-files> nil path))
  ([bucket-name path]
   (if path
     (firebase-storage/listAll (ref bucket-name path))
     (u/resolve> nil))))

(defn list-files-paths>
  ([path]
   (list-files-paths> nil path))
  ([bucket-name path]
   (p/let [^js result (list-files> bucket-name path)
           items (when result (-> result .-items))
           paths (->> items
                      (map (fn [^js item]
                             (.-fullPath item))))]
     paths)))

(defn upload-file> [file path metadata]
  (let [metadata (assoc metadata :contentType (or (-> file .-type)
                                                  (-> metadata :contenType)))]
    (log ::upload-file>
         :file file
         :path path
         :metadata metadata)
    (p/let [ref (ref path)
            result (firebase-storage/uploadBytes ref file (clj->js metadata))]
      result)))

(defn delete> [path]
  (firebase-storage/deleteObject (ref path)))

(ns spark.firebase.storage
  (:require

   ["firebase/storage" :as firebase-storage]

   [spark.logging :refer [log]]
   [spark.env-config :as env-config]))

;; https://firebase.google.com/docs/reference/js/firebase.storage.Reference

(defn- initialize []
  (log ::initialize)
  (let [firebase-app (env-config/get! :firebase-app)
        service (firebase-storage/getStorage firebase-app)]
    (when ^boolean goog.DEBUG
      (firebase-storage/connectStorageEmulator service "localhost" 9199))
    service))

(def storage (memoize initialize))

(defn storage-ref []
  (firebase-storage/ref (storage)))

(defn ref [path]
  (cond
    (string? path)
    (firebase-storage/ref (storage) path)

    (vector? path)
    (reduce (fn [ref path-element]
              (when ref
                (when path-element
                  (firebase-storage/ref ref (str path-element)))))
            (storage-ref) path)

    :else
    path ; asuming it is already a ref
    #_(throw (ex-info "Unsupported path type"
                      {:path path
                       :type (type path)}))))

(defn url> [path]
  (js/Promise.
   (fn [resolve _reject]
     (let [ref (ref path)]
       (if ref
         (-> (firebase-storage/getDownloadURL ref)
             (.then resolve #(resolve nil)))
         (resolve nil))))))

(defn list-files> [path]
  (firebase-storage/listAll (ref path)))

(defn list-files-paths> [path]
  (-> (list-files> path)
      (.then (fn [^js result]
               (->> result
                    .-items
                    (map (fn [^js item]
                           (.-fullPath item))))))))

(defn upload-file> [file path]
  (log ::upload-file>
       :file file
       :path path)
  (firebase-storage/uploadBytes (ref path) file))

(defn delete> [path]
  (firebase-storage/deleteObject (ref path)))

(ns spark.firebase-storage
  (:require
   [spark.logging :refer [log]]))

;; https://firebase.google.com/docs/reference/js/firebase.storage.Reference

(defn storage []
  (let [storage (-> js/firebase .storage)]
    (when goog.DEBUG
      (-> ^js storage (.useEmulator "localhost" 9199)))
    storage))


(defn storage-ref []
  (-> ^js (storage) .ref))


(defn ref [path]
  (cond
    (string? path) (-> ^js (storage-ref) (.child path))

    (vector? path)
    (reduce (fn [^js ref path-element]
              (when ref
                (when path-element
                  (.child ref (str path-element)))))
            (storage-ref) path)

    :else
    path ; asuming it is already a ref
    #_(throw (ex-info "Unsupported path type"
                    {:path path
                     :type (type path)}))))


(defn url> [path]
  (js/Promise.
   (fn [resolve _reject]
     (let [^js ref (ref path)]
       (if ref
         (-> ref
             .getDownloadURL
             (.then resolve #(resolve nil)))
         (resolve nil))))))


(defn list-files> [path]
  (-> ^js (ref path) .listAll))


(defn upload-file> [file path]
  (log ::upload-file>
       :file file
       :path path)
  (js/Promise.
   (fn [resolve reject]
     (let [upload-task (-> ^js (ref path) (.put file))]
       (-> ^js upload-task
           (.on "state_changed"
                #(log ::upload-file>--state_changed
                      :state %)
                reject
                resolve))))))

(defn delete> [path]
  (-> ^js (ref path)
      .delete))

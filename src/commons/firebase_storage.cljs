(ns commons.firebase-storage
  (:require
   [commons.logging :refer [log]]))


(defn storage []
  (-> js/firebase
      .storage))


(defn storage-ref []
  (-> ^js (storage) .ref))


(defn ref [path]
  (cond
    (string? path) (-> ^js (storage-ref) (.child path))

    (vector? path)
    (reduce (fn [^js ref path-element]
              (.child ref (str path-element)))
            (storage-ref) path)

    :else
    path ; asuming it is already a ref
    #_(throw (ex-info "Unsupported path type"
                    {:path path
                     :type (type path)}))))


(defn url> [path]
  (js/Promise.
   (fn [resolve _reject]
     (-> ^js (ref path)
         .getDownloadURL
         (.then resolve #(resolve nil))))))


(defn list-files> [path]
  (-> ^js (ref path) .listAll))


(defn upload-file> [file path]
  (log ::upload-photo-file-selected
       :file file)
  (js/Promise.
   (fn [resolve reject]
     (let [upload-task (-> ^js (ref path) (.put file))]
       (-> ^js upload-task
           (.on "state_changed"
                #(log ::upload-file.state :state %)
                reject
                resolve))))))

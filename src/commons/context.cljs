(ns commons.context
  (:require
   [clojure.spec.alpha :as s]
   [cljs-bean.core :as cljs-bean]
   ["react" :as react]
   [helix.hooks :as hooks]
   [camel-snake-kebab.core :as csk]

   ["react-router-dom" :as router]
   [spark.react :refer [use-effect]]

   [commons.logging :refer [log]]
   [commons.auth :as auth]
   [commons.models :as models]
   [commons.firestore-hooks :as firestore-hooks]
   [commons.firebase-storage :as storage]

   ))



(def use-state react/useState)
(def create-context react/createContext)
(def use-context react/useContext)


(defn use-doc_
  "React hook for a document."
  [path]
  (let [DATA (firestore-hooks/doc-sub path)
        [doc set-doc] (use-state @DATA)
        watch-ref (random-uuid)]

    (use-effect
     :always
     (set-doc @DATA)
     (add-watch DATA
                watch-ref
                (fn [_ _ _ nv]
                  (set-doc nv)))

     #(remove-watch DATA watch-ref))

    doc))


(defn use-col_
  "React hook for a collection."
  [path]
  (log ::use-col
       :path path)
  (let [DATA (firestore-hooks/col-sub path)
        [docs set-docs] (use-state @DATA)
        watch-ref (random-uuid)]

    (use-effect
     :always
     (set-docs @DATA)
     (add-watch DATA
                watch-ref
                (fn [_ _ _ nv]
                  (set-docs nv)))

     #(remove-watch DATA watch-ref))

    docs))


(defn use-cols-union_
  "React hook for a union of collections."
  [paths]
  (log ::use-cols-union
       :paths paths)
  (->> paths
       (reduce (fn [ret path]
                 (let [docs (use-col_ path)]
                   (reduce (fn [ret doc]
                             (assoc ret (-> doc  :id) doc))
                           ret docs)))
               {})
       vals))


(defn use-col [Col]
  (if (map? Col)
    (use-col_ (models/col-path Col))
    (use-col_ Col)))

(defn use-col-subset [col-subset args]
  (if (models/col-subset-is-union? col-subset)
    (use-cols-union_ (models/col-subset-union-paths col-subset args))
    (use-col_ (models/col-subset-path col-subset args))))

(defn use-doc
  ([path]
   (use-doc_ path))
  ([col-model doc-id]
   (use-doc_ (when doc-id [(models/col-path col-model) doc-id]))))




;; TODO deprecated
(defn use-params []
  (->> (router/useParams)
       cljs-bean/->clj
       (reduce (fn [m [k v]]
                 (assoc m (csk/->kebab-case k) v))
               {})))

;; TODO deprecated
(defn use-param [param-key]
  (-> (use-params) (get param-key)))


(defn use-params-2 []
  (->> (router/useParams)
       cljs-bean/->clj
       (reduce (fn [m [k v]]
                 (assoc m k v))
               {})))

(defn use-param-2 [param-key]
  (-> (use-params-2) (get param-key)))


(defn atom-hook
  ([ATOM]
   (atom-hook ATOM identity))
  ([ATOM transformator]
   (fn use-atom []
     (let [[value set-value] (hooks/use-state @ATOM)
           watch-key (random-uuid)]

       (hooks/use-effect
        :once
        (set-value @ATOM)
        (add-watch ATOM watch-key
                   (fn [_k _r ov nv]
                     (when-not (= ov nv)
                       (set-value nv))))
        #(remove-watch ATOM watch-key))

       (transformator value)))))


(def use-auth-user (atom-hook auth/USER))

(defn use-uid []
  (when-let [user (use-auth-user)]
    (-> ^js user .-uid)))

;;;
;;; storage
;;;


(defn use-storage-files [path]
  (let [[files set-files] (use-state [])
        reload-f (fn []
                   (-> (storage/list-files> path)
                       (.then (fn [^js result]
                                (set-files (-> result .-items js->clj))))))]

    (hooks/use-effect
     :once
     (reload-f)
     nil)

    [files reload-f]))


(defn use-storage-url [path]
  (let [[url set-url] (use-state nil)]

    (hooks/use-effect
     :always
     (-> (storage/url> path)
         (.then set-url))
     nil)

    url))

#_(defn use-storage-urls [path]
  (let [[urls set-urls] (use-state [])
        [files reload-files] (use-storage-files path)]

    (hooks/use-effect
     :always
     (-> (js/Promise.all
          (map storage/url> files))
         (.then set-urls))
     nil)

    [urls reload-files]))

;;;
;;; auth
;;;


(def use-auth-completed (atom-hook auth/AUTH_COMPLETED))

;;;
;;; page and context data
;;;

(def DATA_RESOLVER (atom nil))


(def page (create-context {:page nil
                           :data nil}))

(defn use-page []
  (let [data-resolver @DATA_RESOLVER
        _ (when-not data-resolver
            (throw (ex-info "DATA_RESOLVER not initialized"
                            {})))
        page (use-context page)
        data (reduce (fn [m [k identifier]]
                       (assoc m k (data-resolver identifier)))
                     {} (-> page :data))]
    (assoc page :data data)))


(defn use-context-data []
   (let [page (use-page)
         params (use-params)
         data (merge params
                     (-> page :data))]
     data))

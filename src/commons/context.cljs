(ns commons.context
  (:require-macros [commons.context])
  (:require
   [cljs-bean.core :as cljs-bean]
   ["react" :as react]
   [helix.hooks :as hooks]
   [camel-snake-kebab.core :as csk]

   ["react-router-dom" :as router]

   [commons.auth :as auth]
   [commons.models :as models]
   [commons.firestore-hooks :as firestore]
   [commons.firebase-storage :as storage]

   ))



(def use-state react/useState)
(def create-context react/createContext)
(def use-context react/useContext)

(def use-cols-union firestore/use-cols-union)


(defn use-col [Col]
  (if (map? Col)
    (firestore/use-col (models/col-path Col))
    (firestore/use-col Col)))

(defn use-col-subset [col-subset args]
  (if (models/col-subset-is-union? col-subset)
    (firestore/use-cols-union (models/col-subset-union-paths col-subset args))
    (firestore/use-col (models/col-subset-path col-subset args))))

(defn use-doc
  ([path]
   (firestore/use-doc path))
  ([col-model doc-id]
   (firestore/use-doc (when doc-id [(models/col-path col-model) doc-id]))))



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

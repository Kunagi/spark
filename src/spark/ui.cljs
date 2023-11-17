;; * ns
(ns spark.ui
  (:require-macros [spark.ui :refer [try>
                                     $ <> use-effect
                                     def-ui def-ui-showcase
                                     use-state provider create-context
                                     use-context
                                     map$
                                     div span center icon imgdiv
                                     grid grid-0 grid-1 grid-2
                                     grid-3 grid-4 grid-5
                                     stack stack-0 stack-1 stack-2
                                     stack-3 stack-4 stack-5
                                     flex flex-0 flex-1 flex-2
                                     flex-3 flex-4 flex-5]]
                   [kunagi.mui.api]
                   [kunagi.mui.core :refer [defnc]]
                   [clojure.string :as str])

  (:require
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]
   [shadow.resource :as resource]
   [cljs-bean.core :as cljs-bean]
   [camel-snake-kebab.core :as csk]
   [promesa.core :as p]
   [helix.core :as helix]
   [spark.time :as time]

   ["react-router-dom" :as router]

   ["@mui/material" :as mui]
   ["@mui/styles" :as mui-styles]

   [kunagi.mui.api :as kui]
   [kunagi.mui.core :as kui.core]
   [kunagi.utils :as ku]
   [kunagi.utils.debug :as debug]
   [kunagi.mui.pages :as pages]

   [kunagi.utils.logging :refer [log]]
   [spark.utils :as u]
   [spark.core :as spark]
   [spark.firestore :as firestore]
   [spark.db :as db]
   [spark.browser :as browser]
   [spark.firebase.functions :as firebase-functions]
   [spark.env :as env]
   [spark.money :as money]
   [spark.local :as local]

   [spark.firebase.storage :as storage]
   [spark.runtime :as runtime]
   [spark.form-ui :as form-ui]
   [spark.auth :as auth]

   [spark.ui.styles :as styles]
   [spark.ui.showcase :as showcase]
   [kunagi.mui.api :as ui]))

;; * basics

(defn fullscreen-center [children]
  (div
   {:display :flex
    :place-items :center
    :place-content :center
    :height "100vh"}
   (div
    children)))

(def sx ui/sx)
(def ->sx ui/->sx)

;; * Misc

(def debug? debug/active?)

;; (def create-ref js/React.createRef)
(def create-ref kui/create-ref)

(def reg-showcase showcase/reg-showcase)

(def StringVectorChips form-ui/StringVectorChips)
(def FormCardArea form-ui/FormCardArea)
(def FieldCardArea form-ui/FieldCardArea)
(def FieldsCardAreas form-ui/FieldsCardAreas)
(def FieldsCard form-ui/FieldsCard)
(def DocFieldCardArea form-ui/DocFieldCardArea)
(def DocFieldCard form-ui/DocFieldCard)
(def DocFieldsCard form-ui/DocFieldsCard)
(def FormDialogsContainer form-ui/FormDialogsContainer)

(def atom-hook kui/atom-hook)
(def memo kui/memo)

(def use-atom kui/use-atom)
(def use-promise kui/use-promise)

(def ErrorBoundary kui/ErrorBoundary)

(def use-url-params ui/use-url-params )
(def use-url-param ui/use-url-param)

(def coerce-link-to ui/coerce-link-to)
(def to-is-remote? ui/to-is-remote?)
(def to-is-applink? ui/to-is-applink?)
(def RouterLink ui/RouterLink)
(def Link ui/Link)



;; * routing

(defn use-params []
  (->> (router/useParams)
       cljs-bean/->clj
       (reduce (fn [m [k v]]
                 (assoc m (csk/->kebab-case k) v))
               {})))

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

(defn use-location []
  (js->clj
   (router/useLocation)
   :keywordize-keys true))

(def use-history router/useHistory)

(defnc Redirect [{:keys [to]}]
  (let [history (use-history)]
    (use-effect
     [to]
     (when to
       (-> ^js history (.push (coerce-link-to to))))
     nil))
  ($ :div))

(defonce HISTORY (atom nil))

(defn redirect
  ([to]
   (redirect to nil))
  ([to suffix]
   (let [to (if (vector? to)
              (str "/ui/" (str/join "/" to) suffix)
              (str to suffix))]
     (-> ^js @HISTORY (.push to))
     nil)))

(defn redirect-with-replace
  ([to]
   (redirect to nil))
  ([to suffix]
   (let [to (if (vector? to)
              (str "/ui/" (str/join "/" to) suffix)
              (str to suffix))]
     (-> ^js @HISTORY (.replace to))
     nil)))

;; * promises

(def-ui Promise [value formatter]
  (let [resolved-value (use-promise [value] value)]
    (when resolved-value
      (if formatter
        (formatter resolved-value)
        resolved-value))))

;; * firebase

(defn log-error> [error]
  (let [message (if (and error
                         (.hasOwnProperty error "message"))
                  (-> error .-message)
                  "Unknown Error")]
    (db/add> "errors" {:message message
                       :error (str error)
                       :stack (when (and error
                                         (.hasOwnProperty error "stack"))
                                (-> error .-stack))
                       :url js/window.location.href
                       :userAgent js/navigator.userAgent
                       :timestamp :db/timestamp
                       :uid (js/localStorage.getItem "spark.uid")})))

(defn log-error [error]
  (log-error> error)
  nil)

(def call-server> firebase-functions/call>)

(defn server-cmd> [cmd args]
  (p/let [result (call-server> "cmdCall" (assoc args :cmd cmd))]
    ;; (log ::server-cmd>--result
    ;;      :result result
    ;;      :error (-> result :_spark-cmd-error))
    (if-let [error (-> result :_spark-cmd-error)]
      (do
        (log ::server-cmd>--error
             :result result)
        (u/reject> error))
      result)))

;; * date / time

(defonce INSTANT
  (do
    (js/setInterval #(reset! INSTANT (time/instant)) 1000)
    (atom (time/instant))))
(def use-instant (atom-hook INSTANT))

;; 10sec refresh
(defonce INSTANT_10
  (do
    (js/setInterval #(reset! INSTANT_10 (time/instant)) (* 1000 10))
    (atom (time/instant))))
(def use-instant-10 (atom-hook INSTANT_10))

;; 1min refresh
(defonce INSTANT_60
  (do
    (js/setInterval #(reset! INSTANT_60 (time/instant)) (* 1000 60))
    (atom (time/instant))))
(def use-instant-60 (atom-hook INSTANT_60))

(defn use-today []
  (time/date (use-instant-60)))

;; * auth

(def sign-out> auth/sign-out>)

(def use-auth-completed (atom-hook auth/AUTH_COMPLETED))

(def use-auth-user (atom-hook auth/AUTH_USER))

(defn use-uid []
  (when-let [user (use-auth-user)]
    (-> user :uid)))

(def sign-in auth/sign-in)

;; * page and context data

(def PAGE (create-context {:page nil :data nil}))

(defn use-page []
  (use-context PAGE))

(def SPARK_CONTEXT (create-context {:spark/spa :MISSING!
                                    :spark/page :MISSING!}))

(defn use-spark-context []
  (use-context SPARK_CONTEXT))

;; * repository

(defn use-doc
  "React hook for a document."
  ([Doc doc-id]
   (spark/assert-doc-schema Doc)
   (use-doc (cond
              (db/doc? doc-id) doc-id
              doc-id
              [(spark/doc-schema-col-path Doc)
               doc-id])))
  ([path]
   ;; (log ::use-doc
        ;; :path path)
   (let [doc (when (db/doc? path) path)
         [doc set-doc] (use-state doc)
         effect-signal (str path)]

     (use-effect
      [effect-signal]
      (when-not (db/doc? path)
        (set-doc nil)
        (when (and path
                   (not (u/seq-contains-nil? path)))
          (log ::use-doc--subscribe
               :path path)
          (let [ref         (firestore/ref path)
                on-snapshot (fn [doc-snapshot]
                              (let [doc (firestore/wrap-doc doc-snapshot)]
                                ;; (log ::doc-snapshot-received
                                ;;      :path path
                                ;;      :doc doc)
                                (set-doc doc)))
                on-error    (fn [^js error]
                              (log-error error)
                              (log ::use-doc--error
                                   :path path
                                   :exception error))
                debug-id [path (u/nano-id)]
                _ (debug/reg-item :doc debug-id)
                firestore-unsubscribe (.onSnapshot ref on-snapshot on-error)
                unsubscribe (fn []
                              (debug/unreg-item :doc debug-id)
                              (firestore-unsubscribe))]

            unsubscribe))))

     (when (and path
                (not (u/seq-contains-nil? path)))
       doc))))

(defn use-docs
  "React hook for multiple documents."
  ([collection-id docs-ids]
   ;; (log ::use-doc
   ;;      :path path)
   (let [collection-id (if (spark/doc-schema? collection-id)
                         (spark/doc-schema-col-path collection-id)
                         collection-id)
         docs-ids (into #{} docs-ids)
         [docs set-docs] (use-state nil)
         effect-signal (str collection-id docs-ids)]

     (use-effect
      [effect-signal]
      (set-docs nil)
      (when (and collection-id
                 docs-ids)
        ;; (log ::use-doc--subscribe
        ;;      :path path)
        (let [DOCS (atom {})
              unsubscribes (if (empty? docs-ids)
                             (do
                               (set-docs [])
                               nil)
                             (->> docs-ids
                                  (map (fn [doc-id]
                                         (when doc-id
                                           (let [path (str collection-id "/" doc-id)
                                                 ref         (firestore/ref path)
                                                 ;; _ (log ::use-docs
                                                 ;;        :path path
                                                 ;;        :ref ref)
                                                 add-doc (fn [doc]
                                                           (swap! DOCS assoc doc-id doc)
                                                           (when (= (count docs-ids) (count @DOCS))
                                                             (set-docs @DOCS)))
                                                 on-snapshot (fn [doc-snapshot]
                                                               (let [doc (firestore/wrap-doc doc-snapshot)]
                                                                 ;; (log ::use-docs--doc-snapshot-received
                                                                 ;;      :path path
                                                                 ;;      ;; :doc doc
                                                                 ;;      :docs (count docs))
                                                                 (add-doc doc)))
                                                 on-error    (fn [^js error]
                                                               (log-error error)
                                                               (log ::use-docs--error
                                                                    :path path
                                                                    :exception error)
                                                               (add-doc nil))
                                                 unsubscribe (.onSnapshot ref on-snapshot on-error)]

                                             unsubscribe))))
                                  doall))]
          #(doseq [unsubscribe (->> unsubscribes (remove nil?))]
             (unsubscribe)))))

     (when docs
       (->> docs-ids
            (map #(get docs %))
            (remove nil?))))))

(defn use-cached-doc
  ([collection-id doc-id]
   (let [collection-id (if (spark/doc-schema? collection-id)
                         (spark/doc-schema-col-path collection-id)
                         collection-id)
         [docs-map set-docs-map] (use-state nil)]
     #_(log ::use-cached-doc
                :doc-id doc-id
                :docs-map-ids (keys docs-map))

     (use-effect
       [collection-id doc-id]

       (when (and collection-id
                  doc-id)
        ;; (log ::use-doc--subscribe
        ;;      :path path)
         (let [path (str collection-id "/" doc-id)
               ref (firestore/ref path)
              ;; _ (log ::use-docs
              ;;        :path path
              ;;        :ref ref)
               on-snapshot (fn [doc-snapshot]
                             (let [doc (firestore/wrap-doc doc-snapshot)]
                              ;; (log ::use-docs--doc-snapshot-received
                              ;;      :path path
                              ;;      ;; :doc doc
                              ;;      :docs (count docs))
                               (set-docs-map (assoc docs-map (-> doc :id) doc))))
               on-error    (fn [^js error]
                             (log-error error)
                             (log ::use-cached-doc--error
                                  :error error
                                  :path path))
               _unsubscribe (.onSnapshot ref on-snapshot on-error)]

           nil)))

     (get docs-map doc-id))))

(defn use-singleton-doc
  [Doc]
  (use-doc (spark/doc-schema-singleton-doc-path Doc)))

(defn use-col
  "React hook for a collection."
  [path]
  ;; (log ::use-col
  ;;      :path path)
  (let [path (db/coerce-path path)
        [docs set-docs] (use-state nil)
        effect-signal   (or (str path) "_nil")]

    (use-effect
     [effect-signal]
     (when path
       (log ::use-col--subscribe
            :path path)
       (set-docs nil)
       (when path
         (let [col-ref     (firestore/ref path)
               on-snap     (fn [^js query-col-snapshot]
                             ;; (log ::query-snapshot-received
                             ;;      :collection path
                             ;;      :count (-> query-col-snapshot .-docs count)
                             ;;      :snapshot query-col-snapshot)
                             (->> ^js query-col-snapshot
                                  .-docs
                                  (map firestore/wrap-doc)
                                  set-docs))
               on-error    (fn [^js firestore-error]
                             ;; FIXME debug
                             (when (debug?)
                               (js/alert firestore-error))
                             (let [msg (str "Loading collection " (u/->edn path) " failed: "
                                            (str firestore-error))
                                   error (js/Error. msg (clj->js {:cause firestore-error}))]
                               (js/console.error error firestore-error)
                               (log-error error)))
               debug-id [path (u/nano-id)]
               _ (debug/reg-item :col debug-id)
               firestore-unsubscribe (.onSnapshot col-ref on-snap on-error)
               unsubscribe (fn []
                             (debug/unreg-item :col debug-id)
                             (firestore-unsubscribe))]
           unsubscribe))))

    docs))

(defn use-cols-union
  "React hook for a union of collections."
  [paths]
  ;; (log ::use-cols-union
  ;;      :paths paths)
  (->> paths
       (reduce (fn [ret path]
                 (let [docs (use-col path)]
                   (reduce (fn [ret doc]
                             (assoc ret (-> doc  :id) doc))
                           ret docs)))
               {})
       vals))

(defn use-query
  [query context]
  ;; (log ::use-query>
  ;;      :query query
  ;;      :context context)

  (let [result (cond
                 (-> query :path)
                 (let [path (-> query :path (u/fn->value context))]
                   (if (-> path count even?)
                     (use-doc path)
                     (use-col path)))

                 (-> query :paths)
                 (use-cols-union (-> query :paths (u/fn->value context)))

                 :else
                 (throw (ex-info "Invalid query. Missing :path or :paths."
                                 {:query query})))]
    (when result
      (runtime/post-process-query-result query context result))))

;; * db

(def db-get> db/get>)
(def transact> db/transact>)
(def db-update> db/update>)
(def db-add> db/add>)
(def db-add-child> db/add-child>)
(def db-delete> db/delete>)

;; * Styles / Theme

(defn use-theme []
  (mui-styles/useTheme))

(defn make-styles [styles-f]
  (mui-styles/makeStyles
   (fn [theme]
     (clj->js (styles-f theme)))))

(defn use-styles-class [styles-f]
  (when styles-f
    (let [theme            (use-theme)
          styles-f-wrapper (fn [theme]
                             {:root (ui/conform-style
                                     (if (fn? styles-f)
                                       (styles-f theme)
                                       styles-f)
                                     false)})
          use-styles       (make-styles styles-f-wrapper)
          ^js styles       (use-styles theme)]
      (-> styles .-root))))

;; * storage

(defn delete-storage-file> [path]
  (storage/delete> path))

(defn use-storage-files
  ([path]
   (use-storage-files nil path))
  ([bucket-name path]
   (let [[files set-files] (use-state nil)
         reload-f          (fn []
                             (p/let [^js result (storage/list-files> bucket-name path)
                                     items (when result (-> result .-items))
                                     paths (->> items
                                                (map (fn [^js item]
                                                       (.-fullPath item))))]
                               (set-files paths)))]

     (use-effect
      [(str path)]
      (reload-f)
      nil)

     [files reload-f])))

(defonce STORAGE_URLS_CACHE (atom {}))
(def use-storage-urls-cache (atom-hook STORAGE_URLS_CACHE))

(defn use-storage-url
  ([path]
   (use-storage-url path nil))
  ([path fallback-url]
   (let [cache (use-storage-urls-cache)]

     (use-effect
      [path]
      (when path
        (when-not (get cache path)
          (log ::use-storage-url--load
               :path path
               :fallback fallback-url)
          (-> (u/=> (storage/url> path)
                    (fn [new-url]
                      ;; (log ::use-storage-url--received
                      ;;      :url new-url
                      ;;      :path path)
                      (swap! STORAGE_URLS_CACHE
                             assoc path (or new-url fallback-url))))
              (.catch #(when fallback-url (swap! STORAGE_URLS_CACHE
                                                 assoc path fallback-url))))))
      nil)

     (if path
       (get cache path)
       fallback-url))))

;; (defn use-storage-url
;;   ([path]
;;    (use-storage-url path nil))
;;   ([path fallback-url]
;;    ;; (log ::use-storage-url
;;    ;;      :path path
;;    ;;      :fallback fallback-url)
;;    (let [[url set-url] (use-state (if path
;;                                     fallback-url
;;                                     nil))
;;          update-url    (fn [url]
;;                          (set-url (or url
;;                                       fallback-url)))]

;;      (use-effect
;;       :always
;;       (let [ACTIVE (atom true)]
;;         (when path
;;           (-> (u/=> (storage/url> path)
;;                     (fn [new-url]
;;                       ;; (log ::use-storage-url--received
;;                       ;;      :url url
;;                       ;;      :path path
;;                       ;;      :active @ACTIVE)
;;                       (when @ACTIVE
;;                         (update-url new-url))))
;;               (.catch #(when @ACTIVE (update-url nil)))))
;;         #(reset! ACTIVE false)))

;;      url)))

;; (defn use-storage-urls
;;   [paths]
;;   (log ::use-storage-urls
;;        :paths paths)
;;   (let [[urls set-urls] (use-state nil)]

;;     (use-effect
;;      :always
;;      (let [ACTIVE (atom true)]
;;        (when (seq paths)
;;          (u/=> (u/all> (map (fn [path]
;;                               (storage/url> path))
;;                             paths))
;;                (fn [new-urls]
;;                  (log ::use-storage-urls--received
;;                       :urls new-urls
;;                       :paths paths
;;                       :active @ACTIVE)
;;                  (when @ACTIVE
;;                    (set-urls new-urls)))))
;;        #(reset! ACTIVE false)))

;;     urls))

;; (defn use-storage-urls [paths]
;;   (when paths
;;     (mapv (fn [path]
;;             (use-storage-url path))
;;           paths)))

;; * styles

;; TODO deprecated
(defn style-bg-img [url]
  {:background-image      (str "url(" url ")")
   :background-repeat     "no-repeat"
   :background-position-x "center"
   :background-position-y "top"
   :background-size       "contain"})

;; * common ui functions

(defn colored-data-block [label background-color color data]
  ($ :div
     {:class "data"
      :style {:white-space      "pre-wrap"
              :word-break "break-all"
              :font-family      "monospace"
              :font-size        "13px"
              :font-weight      400
              :font-style       "normal"
              :overflow         "auto"
              :width            "100%"
              :background-color background-color
              :color            color
              :padding          "1rem"
              :border-radius    "4px"
              :margin           "1px"}}
     (when label
       (div
        {:font-weight 900
         :text-align  "center"}
        (with-out-str (print label))))
     (with-out-str (pprint data))))

(defn colored-data-line [label background-color color data]
  ($ :div
     {:style {:white-space "nowrap"
              :height "50px"
              :font-family :monospace
              :width "100%"
              :background-color background-color
              :color color
              :padding "1rem"
              :border-radius "4px"
              :margin "1px"
              :display :grid
              :place-items "center"}}
     (div
      {:max-width "300px"
       :overflow "hidden"
       :text-overflow "ellipsis"}
      (when label
        ($ :span
           {:style
            {:font-weight 900
             :text-align "center"}}
           (with-out-str (print label))
           " "))
      (with-out-str (print data)))))

(defn data [& datas]
  ($ :div
     (for [[i data] (map-indexed vector datas)]
       ($ :div
          {:key i}
          (kui/data data)))))

(defn data-card-content [text v]
  ($ mui/CardContent
     (when text
       (div text))
     (data v)))

(def DEBUG kui/DEBUG)
(def DEBUG_ kui/DEBUG_)

(defnc ExpandableData [{:keys [label value]}]
  (let [[expanded set-expanded] (use-state false)]
    (if expanded
      (colored-data-block label "#333" "#6f6" value)
      ($ :div
         {:onClick #(set-expanded true)
          :style   {:cursor "pointer"}}
         (colored-data-line label "#333" "#6f6" value)))))

(defn expandable-data [label value]
  ($ ExpandableData
     {:label label
      :value value}))

(defn spacer
  ([size]
   (spacer size size))
  ([width height]
   (div
    {:width  width
     :height height})))

(defn v-scroll-pane [{:keys [without-bars?]} content]
  (ui/div
   {:overflow-y :auto
    :overflow-x :hidden
    ;; :background-color "yellow"
    :border-top (when-not without-bars? "1px solid rgba(0,0,0,0.3)")
    :border-bottom (when-not without-bars? "1px solid rgba(0,0,0,0.3)")}
   content))

(defn rect-in-viewport? [rect]
  (and (>= js/window.screen.height (-> rect .-bottom))
       (>= js/window.screen.width (-> rect .-right))
       (>= (-> rect .-bottom) 0)
       (>= (-> rect .-right) 0)))

(defn element-in-viewport? [^js element]
  (-> element .getBoundingClientRect rect-in-viewport?))

(defn use-screen-enter [^js ref callback]
  (let [[in set-in] (use-state false)
        activate (fn []
                   ;; (js/console.log "!!!!! scroll")
                   (when (-> ref .-current)
                     (if (-> ref .-current .getBoundingClientRect rect-in-viewport?)
                       (when-not in
                         (set-in true)
                         (callback))
                       (when in
                         (set-in false)))))]

    (use-effect
     :always
     (js/document.addEventListener "scroll" activate)
     #(js/document.removeEventListener "scroll" activate))))

;; * test helpers

(defn tdiv [color]
  ($ :div
     {:style {:background-color color
              :min-width        "64px"
              :min-height       "16px"}}))

(defn tdiv-red [] (tdiv "#c62828"))
(defn tdiv-blue [] (tdiv "#1565c0"))
(defn tdiv-green [] (tdiv "#2e7d32"))
(defn tdiv-yellow [] (tdiv "#f9a825"))

(def-ui-showcase ::grid
  (stack
   (grid [:auto :auto] (tdiv-red) (tdiv-blue))
   (grid-0 [:auto :auto] (tdiv-red) (tdiv-blue))
   (grid-3 [:auto :auto] (tdiv-red) (tdiv-blue))
   (grid [:auto :auto] {:grid-gap 10} (tdiv-red) (tdiv-blue))
   (grid [:auto "200px" :auto] (tdiv-red) (tdiv-yellow) (tdiv-blue))
   (div
    {:width "200px"}
    (grid ["repeat(auto-fit, minmax(64px, 1fr))"]
          (tdiv-red) (tdiv-yellow) (tdiv-blue) (tdiv-green)
          (tdiv-red) (tdiv-yellow) (tdiv-blue) (tdiv-green)))))

(defn unsplash [width id]
  (str "https://images.unsplash.com/photo-" id "?&w=" width))

(def flex-filler (div {:flex 1}))

;;;
;;; def-cmp
;;;

(def-ui-showcase ::def-ui
  (stack
   (do (def-ui TestComponent-1 []
         "hello world")
       ($ TestComponent-1))

   (do (def-ui TestComponent-2 [{:keys [uid greeting]}]
         {:from-context [uid]}
         (str greeting " " uid "!"))
       ($ TestComponent-2 {:greeting "hello"}))

   (do (def-ui TestComponent-3 [uid greeting]
         {:from-context [uid]}
         (str greeting " " uid "!"))
       (stack
        ($ TestComponent-3 {:greeting "hello"})
        (data (macroexpand-1 '(def-ui TestComponent-3 [uid greeting]
                                {:from-context [uid]}
                                (str greeting " " uid "!"))))))))

;;;
;;; common components
;;;

(defn alert--error [content]
  ($ mui/Alert
     {:severity "error"}
     (ui/div
      {:white-space :pre-wrap}
      (local/textc content))))

(defn alert--warning [content]
  ($ mui/Alert
     {:severity "warning"}
     (ui/div
      {:white-space :pre-wrap}
      (local/textc content))))

(defn alert--info [content]
  ($ mui/Alert
     {:severity "info"}
     (ui/div
      {:white-space :pre-wrap}
      (local/textc content))))

(defn alert--success [content]
  ($ mui/Alert
     {:severity "success"}
     (ui/div
      {:white-space :pre-wrap}
      (local/textc content))))

(defnc ScrollToTop [{:keys []}]
  (let [location (use-location)]

    (use-effect
     [location]
     ;; (log ::ScrollToTop--scroll)
     (js/window.scrollTo 0 0)

     nil)

    (div {:id "ScrollToTop"})))

(defnc Spacer [{:keys [width height]}]
  (let [theme (mui-styles/useTheme)]
    ($ :div
       {:style {:width  (-> theme (.spacing (or width 1)))
                :height (-> theme (.spacing (or width 1)))}})))

(defnc Icon [{:keys [name theme]}]
  ($ :div
     {:class (str "material-icons"
                  (when theme (str "-" theme)))}
     name))

(defnc Loader [{:keys [message height]}]
  (div
   {:display :grid
    :place-items :center
    :place-content :center
    :min-height (or height
                    "100px")}
   (stack
    {:padding 16
     :text-align :center
     :color "rgba(0,0,0,0.4)"}
    (center ($ mui/CircularProgress))
    (when message
      (center
       (div {:max-width "300px"} message))))))

(defnc ValueLoadGuard [{:keys [children value message height pass-through]}]
  (if (or pass-through value)
    (ui/<>
     children)
    ($ Loader {:message message
               :height height})))

(defnc ValuesLoadGuard [{:keys [children values message height]}]
  (if (reduce (fn [ret value]
                (and ret value))
              true values)
    children
    ($ Loader {:message message
               :height height})))

(defnc Stack [{:keys [children spacing]}]
  (let [theme (mui-styles/useTheme)]
    ($ :div
       {:style {:display :grid
                :grid-gap (-> theme (.spacing (or spacing 1)))}}
       children)))

(defnc Flexbox [{:keys [children spacing style]}]
  (let [children (if (seqable? children)
                   (->> children (remove nil?))
                   [children])
        theme    (mui-styles/useTheme)]
    ($ :div
       {:style {:display :flex
              ;; FIXME :gap (-> theme (.spacing (or spacing 1)))
                }}
       (for [[idx child] (map-indexed vector children)]
         ($ :div
            {:key   idx
             :style (merge  {:margin-right (-> theme (.spacing (or spacing 1)))}
                            style)}
            child)))))

(defnc Link--no-styles [{:keys [to href on-click children]}]
  ($ Link
     {:to        to
      :href      href
      :on-click  on-click
      :className "Link--no-styles"}
     children))

;;; SPA

(def use-offline (atom-hook browser/OFFLINE))

(defn use-height []
  (let [[height set-height] (ui/use-state js/window.innerHeight)]

    (ui/use-effect
      :once
      (let [el #(set-height js/window.innerHeight)]
        (js/window.addEventListener "resize" el)
        #(js/window.removeEventListener "resize" el)))

    height))

(defn use-width []
  (let [[width set-width] (ui/use-state js/window.innerWidth)]

    (ui/use-effect
     :once
     (let [el #(set-width js/window.innerWidth)]
       (js/window.addEventListener "resize" el)
       #(js/window.removeEventListener "resize" el)))

    width))

(defn use-page-y-offset []
  (let [[offset set-offset] (ui/use-state js/window.pageYOffset)]

    (ui/use-effect
      :once
      (let [el #(set-offset js/window.pageYOffset)]
        (js/window.addEventListener "scroll" el)
        #(js/window.removeEventListener "scroll" el)))

    offset))

(defonce SPA (atom nil))
(def use-spa (atom-hook SPA))

(defn use-app-styles-class []
  (let [spa (use-spa)]
    (use-styles-class (-> spa :styles))))

(def-ui LazyChildren [children]
  (let [chunk-size 10
        [limit set-limit] (ui/use-state chunk-size)
        limited? (-> children count (> limit))
        items (if limited? (take limit children) children)

        loader-id (str "LazyChildrenLoader_" (random-uuid))
        scroll-offset (use-page-y-offset)
        visible-top (+ scroll-offset js/window.innerHeight)
        ]

    (ui/use-effect
     :always
     (when limited?
       (when-let [e (js/document.getElementById loader-id)]
         (when-let [e-top (-> e .getBoundingClientRect .-top)]
           (when (-> e-top (<= visible-top))
             (set-limit (+ limit chunk-size))))))
     nil)

    (ui/<>
     items
     (ui/div {:id loader-id}))))

;; * dialogs

(defonce DIALOGS (atom {}))

(def use-dialogs (atom-hook DIALOGS))

(defn show-dialog> [dialog]
  (u/promise>
   (fn [resolve _reject]
     (let [id     (or (-> dialog :id)
                      (str "dialog_" (u/nano-id)))
           dialog (assoc dialog
                         :id id
                         :open? true
                         :on-close (fn []
                                     (when-let [on-close (-> dialog :on-close)]
                                       (on-close))
                                     (resolve)))]
       (swap! DIALOGS assoc id dialog)
       id))))

(defn show-dialog [dialog]
  (show-dialog> dialog)
  nil)

(defn hide-dialog [id]
  (log ::hide-dialog
       :id id)
  (swap! DIALOGS update id (fn [dialog]
                             (when-let [on-close (-> dialog :on-close)]
                               (on-close))
                             (assoc dialog :open? false)))
  (js/setTimeout #(swap! DIALOGS dissoc id)
                 1000))

(defonce DIALOG_ID (create-context nil))

(defn use-dialog-id []
  (use-context DIALOG_ID))

(defn use-hide-dialog []
  (let [dialog-id (use-dialog-id)]
    (when dialog-id
      #(hide-dialog dialog-id))))

(defnc Dialog [{:keys [dialog]}]
  {:wrap [memo]}
  (let [class     (use-app-styles-class)
        dialog-id (-> dialog :id)]
    (provider
     {:context DIALOG_ID
      :value   dialog-id}
     ($ mui/Dialog
        {:open      (-> dialog :open?)
         :onClose   #(hide-dialog (-> dialog :id))
         :className (str class " " (-> dialog :class))
         :maxWidth (get dialog :max-width "sm")
         ;; :sx (clj->js {"background-color" "blue"
         ;; "outline" "2px soldi blue"}
         ;; )
         :sx (->sx (-> dialog :sx))}
        ;; (data dialog-id)
        ;; (DEBUG dialog)
        (when-let [title (-> dialog :title)]
          (div
           (when (-> dialog :title-close-button)
             (div
              {:position :absolute
               :right 0
               :top 0}
              (ui/div
               {:padding-top 12
                :padding-right 12
                :padding-left 12}
               (if (string? (-> dialog :title-close-button))
                 ($ mui/Button
                    {:onClick #(hide-dialog (-> dialog :id))
                     :variant "contained"
                     :color "primary"
                     :className (-> dialog :title-close-button-class)}
                    (-> dialog :title-close-button))
                 ($ mui/IconButton
                    {:onClick #(hide-dialog (-> dialog :id))}
                    (div {:class "material-icons"} "close"))))))
           ($ mui/DialogTitle
              (div
               {:padding-right 32}
               title))))
        ($ mui/DialogContent
           (stack
            (-> dialog :content)
            (when (or (-> dialog :cancel-button)
                      (-> dialog :on-cancel))
              (center
               ($ mui/Button
                  {:onClick (fn []
                              (hide-dialog (-> dialog :id))
                              (when-let [on-cancel (-> dialog :on-cancel)]
                                (on-cancel)))}
                  (or (-> dialog :cancel-button-text)
                      (local/text :cancel)))))))))))

(defnc DialogsContainer []
  (let [dialogs (-> (use-dialogs) vals)]
    (<>
     (for [dialog dialogs]
       ($ Dialog {:key (-> dialog :id) :dialog dialog})))))

(def show-form-dialog form-ui/show-form-dialog)
(def show-form-dialog> form-ui/show-form-dialog>)
(def use-hide-form-dialog form-ui/use-hide-form-dialog)

(def-ui MessageDialogContent [message opts]
  (let [hide (use-hide-dialog)
        options (->> opts
                     :options
                     (remove nil?))]
    (stack-3
     (ui/div
      {:white-space :pre-wrap}
      message)
     (if (seq options)
       (center
        (ui/DEBUG opts)
        (stack
         (for [option options]
           ($ mui/Button
              {:key (or (-> option :id)
                        (-> option :text))
               :variant "contained"
               :color "primary"
               :onClick (fn []
                          (hide)
                          ((-> option :on-click)))}
              (-> option :text)))
         (when (-> opts :cancel-button)
           ($ mui/Button
              {:onClick hide}
              (local/text :cancel)))))

       (center
        (ui/flex
         (when (-> opts :cancel-button)
           ($ mui/Button
              {:onClick hide
               :color "default"}
              (local/text :cancel)))
         (when (get opts :ok-button true)
           ($ mui/Button
              {:onClick hide}
              (or (-> opts :ok-text)
                  (local/text :ok))))))))))

(defn show-message-dialog
  "opts: `title`, `title-close-button`"
  ([message]
   (show-message-dialog message {}))
  ([message opts]
   (show-dialog {:title (-> opts :title)
                 :title-close-button (-> opts :title-close-button)
                 :content ($ MessageDialogContent
                             {:message message
                              :opts opts})})))

(defn show-message-dialog>
  "opts: `title`, `title-close-button`"
  ([message]
   (show-message-dialog> message {}))
  ([message opts]
   (show-dialog> {:title (-> opts :title)
                 :title-close-button (-> opts :title-close-button)
                 :content ($ MessageDialogContent
                             {:message message
                              :opts opts})})))

;; ** PromiseProgress

(def-ui PromiseProgress [promise]
  (let [hide (use-hide-dialog)]

    (use-effect
     :once
     (-> promise
         (.then (fn [result]
                  (hide)
                  result))
         (.catch (fn [error]
                   (hide)
                   (u/reject> error))))
     nil)

    (stack
     ($ mui/CircularProgress))))

(defn eval-with-progress-dialog [f>]
  (let [promise (if (fn? f>) (f>) f>)]
    (when (instance? js/Promise promise)
      (show-dialog
       {:content (div
                  {:min-width "100px"
                   :min-height "100px"
                   :display :grid
                   :place-items :center
                   :place-content :center}
                  ($ PromiseProgress {:promise promise}))}))
    promise))

(defn with-progress-dialog [f]
  (when f
    #(eval-with-progress-dialog f)))

;;; * forms

(def assoc-to-field spark/assoc-to-opts)

;;; * errors

(defonce ERROR (atom nil))

(def use-error (atom-hook ERROR))

(defn show-error [error]
  (runtime/report-error error)
  (reset! ERROR error))

(def-ui ErrorInfo [error]
  ($ mui/Card
     ($ mui/CardContent
        ($ kui.core/ErrorInfo {:error error}))))

(def-ui-showcase ::ErrorInfo
  (stack
   ($ ErrorInfo {:error "Just Text"})
   ($ ErrorInfo {:error (ex-info "Clojure Exception with Data"
                                 {:with "data"
                                  :and :info})})
   ($ ErrorInfo {:error (ex-info "Clojure Exception with Cause"
                                 {}
                                 (ex-info "Root Cause" {}))})
   ($ ErrorInfo {:error (js/Error. "JavaScript Error")})))

(defnc ErrorDialog []
  (let [error (use-error)]
    ($ mui/Dialog
       {:open (-> error boolean)
        :onClose #(reset! ERROR nil)
        :maxWidth "xl"
        :fullWidth true}
       ($ mui/DialogTitle
          "Error")
       ($ mui/DialogContent
          ($ ErrorInfo {:error error}))
       ($ mui/DialogActions
          ($ mui/Button
             {:onClick #(reset! ERROR nil)}
             "Close")))))

(defn wrap-in-error-handler [f]
  (when f
    (fn [& args]
      (try
        (let [result (apply f args)]
          (if (instance? js/Promise result)
            (-> result
                (.catch (fn [error]
                          (show-error error)
                          error)))
            result)
          result)
        (catch :default error (show-error error))))))

;;; links

(defnc LinkCardActionArea [{:keys [to children]}]
  ($ mui/CardActionArea
     {:to        to
      :component RouterLink}
     children))

(defnc SimpleLinkCard [{:keys [to children]}]
  ($ mui/Card
     ($ LinkCardActionArea {:to to}
        ($ mui/CardContent
           (stack
            children)))))

;;; commands

(defn upgrade-legacy-command [command]
  (if (-> command :type)
    command
    (if (-> command :onClick)
      command
      (if (-> command :form)
        (do
          (js/console.error "Missing :type in form command:" command)
          (assoc command :type :form))
        command))))

(defn- complete-form [form context]
  (let [form (if (fn? form)
               (form context)
               form)
        form (update form
                     :values
                     #(merge (-> context :form-defaults)
                             %))]
    form))

(defn execute-command>
  ([command context]
   (execute-command> command context nil))
  ([command context then]
   ;; (log ::execute-command>
   ;;      :command command
   ;;      :then then)
   (js/console.log "[DEBUG] ui/execute-command>")
   (-> (runtime/execute-command> command context)
       (.then (or then identity))
       (.catch show-error))))

(defn new-command-on-click [command context then]
  ;; (runtime/validate-command command)
  (let [ui-context (use-spark-context)
        form       (-> command :form)]
    (if-not form
      #(execute-command> command (merge ui-context context) then)
      #(let [context (merge ui-context context)
             _       (runtime/validate-command-context command context)
             form    (complete-form form context)
             submit  (fn [values]
                       ;; (log ::new-command-on-click--form-submit
                       ;;      :values values)
                       (execute-command>
                        command
                        (assoc context :values values)
                        then))
             form    (assoc form :submit submit)]
         (show-form-dialog> form)))))

;; TODO deprecated
(defnc CommandButton [{:keys [command context then
                              icon as-icon? icon-theme
                              variant color size
                              class styles
                              text]}]
  (let [command      (u/trampoline-if command)
        on-click     (new-command-on-click command context then)
        on-click     (wrap-in-error-handler on-click)
        on-click     (with-progress-dialog on-click)
        variant      (or variant "contained")
        color        (or color
                         (when (-> command :inconspicuous?) "default")
                         "primary")
        icon         (when-let [icon (or icon
                                         (-> command :icon))]
                       ($ Icon {:name  icon
                                :theme icon-theme}))
        styles-class (use-styles-class styles)
        classes      (str/join " " [class styles-class])]
    (if as-icon?
      ($ mui/IconButton
         {:onClick   on-click
          :color     color
          :size      size
          :className classes}
         icon)
      ($ mui/Button
         {:onClick   on-click
          :variant   variant
          :color     color
          :startIcon icon
          :size      size
          :className classes}
         (or text
             (spark/cmd-label command))))))

(def-ui-showcase ::CommandButton
  (stack
   ($ CommandButton {:command {:label "default" :f (fn [_] [])}})
   ($ CommandButton {:command {:label "inconspicuous" :f (fn [_] [])
                               :inconspicuous? true}})
   ($ CommandButton {:command {:label "with icon" :f (fn [_] [])
                               :icon "thumb_up"}})
   ($ CommandButton {:command {:label "only icon" :f (fn [_] [])
                               :icon "thumb_up"}
                     :as-icon? true})
   ($ CommandButton {:command {:label "color: secondary" :f (fn [_] [])}
                     :color "secondary"})))

(defnc CommandCardArea [{:keys [command children context then]}]
  (let [command (u/trampoline-if command)
        onClick (when command
                  (wrap-in-error-handler (new-command-on-click command context then)))]
    ($ mui/CardActionArea
       {:onClick onClick
        :disabled (nil? onClick)}
       children)))

(defnc CommandCard [{:keys [command children context then]}]
  ($ mui/Card
     ($ CommandCardArea
        {:command command
         :context context
         :then then}
        children)))

(defn- complete-command [command]
  (case (-> command :type)

    :form
    (assoc command
           :f
           (fn [context]
             [[:fn #(show-form-dialog
                     (-> command :form (complete-form context)))]]))

    command))

(defn- wrap-on-click-in-hide-dialog [f hide-dialog]
  (if-not hide-dialog
    f
    (fn [event]
      (let [result (f event)]
        (if-not (instance? js/Promise result)
          (hide-dialog)
          (u/=> result
                (fn [result]
                  (hide-dialog)
                  result)))))))

(defn event-prevent-default [^js event]
  (when event
    (-> event .preventDefault)
    (when (-> event .-stopImmediatePropagation)
      (-> event .stopImmediatePropagation))
    (-> event .stopPropagation)))

(defnc Button [{:keys [text icon
                       onClick on-click to href target
                       pre-on-click
                       variant color size disabled
                       command
                       context
                       then
                       class styles sx
                       id
                       auto-hide-dialog]}]
  (when command
    (log ::Button.DEPRECATED.with-command
         :command command))
  (let [context  (merge (use-spark-context)
                        context)
        command  (u/trampoline-if command)
        command  (when command (-> command upgrade-legacy-command complete-command))
        text     (or text (-> command :label))
        text (when text (local/textc text))
        icon     (when-let [icon (or icon (-> command :icon))]
                   (cond
                     (string? icon)  ($ :div {:class "material-icons"} icon)
                     (keyword? icon) ($ :div {:class "material-icons"} (name icon))
                     :else           icon))
        on-click (or on-click onClick)
        on-click (or on-click
                     (-> command :onClick)
                     (when command
                       #(execute-command> command context then)))

        on-click (with-progress-dialog on-click)
        on-click (wrap-in-error-handler on-click)

        [to on-click] (if (and to hide-dialog)
                        [nil #(redirect to)]
                        [to on-click])

        hide-dialog (use-hide-dialog)
        on-click    (if auto-hide-dialog
                      (wrap-on-click-in-hide-dialog on-click hide-dialog)
                      on-click)

        on-click (when on-click
                   (fn [^js event]
                     (event-prevent-default event)
                     (on-click)))

        variant      (if (keyword? variant) (name variant) variant)
        color        (if (keyword? color) (name color) color)
        color        (or color
                         (when (-> command :inconspicuous?) "default")
                         "primary")
        styles-class (use-styles-class styles)
        classes      (str/join " " [class styles-class])

        size (if (keyword? size) (name size) size)]
    (if to
      (let [to (coerce-link-to to)]
        (if (and icon (not text))
          ($ mui/IconButton
             {:to        to
              :component router/Link
              :id        id
              :disabled  disabled
              :variant   (or variant "contained")
              :color     (or color "primary")
              :size      size
              :className classes
              :sx (->sx sx)}
             icon)
          ($ mui/Button
             {:to        to
              :component router/Link
              :id        id
              :disabled  disabled
              :variant   (or variant "contained")
              :color     (or color "primary")
              :startIcon icon
              :size      size
              :className classes
              :sx (->sx sx)}
             text)))
      (if (and icon (not text))
        ($ mui/IconButton
           {:onClick   on-click
            :id        id
            :disabled  disabled
            :href      href
            :target    target
            :variant   (or variant "contained")
            :color     (or color "primary")
            :size      size
            :className classes
            :sx (->sx sx)}
           icon)
        ($ mui/Button
           {:onClick   on-click
            :id        id
            :disabled  disabled
            :href      href
            :target    target
            :variant   (or variant "contained")
            :color     (or color "primary")
            :startIcon icon
            :size      size
            :className classes
            :sx (->sx sx)}
           text)))))

(defnc IconButton [{:keys [onClick on-click
                           icon color size command theme className
                           then context]}]
  (let [context  (merge (use-spark-context)
                        context)
        on-click (or on-click onClick)
        command  (u/trampoline-if command)
        command  (when command (-> command upgrade-legacy-command complete-command))
        on-click (wrap-in-error-handler
                  (or on-click
                      (when command (-> command :onClick))
                      (when command #(execute-command> command context then))))
        on-click (with-progress-dialog on-click)
        on-click (wrap-in-error-handler on-click)
        icon     (when-let [icon (or icon
                                     (-> command :icon)
                                     "play_arrow")]
                   (cond
                     (keyword? icon)
                     ($ :div {:class (str "material-icons" (when theme (str "-" theme)))}
                        (name icon))

                     (string? icon)
                     ($ :div {:class (str "material-icons" (when theme (str "-" theme)))}
                        icon)

                     :else icon))]
    ($ mui/IconButton
       {:className className
        :onClick   on-click
        :color     color
        :size      size}
       icon)))

(defnc CardOverline [{:keys [text]}]
  ($ mui/Typography
     {:variant "overline"}
     text))

(defnc SimpleCard [{:keys [title children className to on-click]}]
  (let [Content ($ mui/CardContent
                   ($ Stack
                      (when title ($ CardOverline {:text title}))
                      ($ Stack children)))]
    ($ mui/Card
       {:className className}
       (cond

         to
         ($ mui/CardActionArea
            {:to        (coerce-link-to to)
             :component RouterLink}
            Content)

         on-click
         ($ mui/CardActionArea
            {:onClick on-click}
            Content)

         :else
         Content))))

(def-ui-showcase ::SimpleCard
  ($ SimpleCard
     {:title "example title"}
     "Example content."))

(defnc CardRow [{:keys [gap children
                        grid-template-columns align-items
                        style sx]}]
  (let [children (when children
                   (if (array? children)
                     (->> children (remove nil?) seq)
                     children))]
    (when children
      ($ :div
         {:class "CardRow"
          :sx (->sx sx)
          :style (merge
                  {:display :grid
                   :grid-template-columns (or grid-template-columns
                                              (str "repeat(" (if (or (array? children)
                                                                     (sequential? children))
                                                               (count children)
                                                               1)
                                                   ", auto)"))
                   :align-items (if (keyword? align-items)
                                  (name align-items)
                                  align-items)
                   :grid-gap gap}
                  style)}
         (clj->js children)))))

(def FieldLabel form-ui/FieldLabel)
(def Field form-ui/Field)


;;;
;;; desktop
;;;

(defnc PageContent []
  (let [page           (use-page)
        use-container? (get page :use-container true)
        Content        ($ ValuesLoadGuard {:values (-> page :data vals)
                                           :height "60vh"
                                           :message "Lade Seiteninhalte"}
                          ($ (-> page :content)))]
    (if use-container?
      ($ mui/Container
         {:maxWidth (get page :max-width "sm")}
         Content)
      Content)))

;; (defonce ADDITIONAL_PAGES (atom nil))

(defn- dev-section [title content]
  (div
   {:padding "8px"}
   (div
    {:font-weight 900
     :color "#6d6d6d"
     :letter-spacing 1}
    title)
   (div content)))

(defn- page-dev-sidebar [page context]
  (div
   {:background-color "#424242"
    :color            "#eee"}
   (flex
    {:padding "8px"}
    ($ Button
       {:to   "/ui/devcards"
        :text "DevCards"
        :size "small"}))
   (dev-section
    "PAGE CONTEXT"
    (div
     (for [[k v] (->> context (sort-by first))]
       (div
        {:key k}
        (expandable-data k v)))))))

(defonce DEV-SIDEBAR-ENABLED (atom false))
(def use-dev-sidebar-enabled (atom-hook DEV-SIDEBAR-ENABLED))

(defnc DevSidebarToggleIconButton []
  ($ mui/IconButton
     {:onClick #(swap! DEV-SIDEBAR-ENABLED not)}
     (icon "developer_mode")))

(defnc DevSidebarPageWrapper [{:keys [page context children]}]
  (div
   {:display               :grid
    :grid-template-columns "1fr 1fr"}
   (div
    children)
   (page-dev-sidebar page context)))

(defn load-docs+context [context spa page params]
  (let [context (assoc context :spark/page page)

        ;; put page params into context
        context (reduce (fn [context [k v]]
                          (assoc context
                                 k v
                                 (csk/->kebab-case-keyword k) v))
                        context params)

        docs (merge (reduce (fn [docs [k Doc]]
                              (let [id  (get context k)
                                    doc (use-doc Doc id)]
                                (assoc docs k doc)))
                            {} (-> page :use-docs))
                    (reduce (fn [docs [k Doc]]
                              (let [param (-> Doc
                                              spark/doc-schema-page-param)
                                    id    (get context param)
                                    doc   (use-doc Doc id)]
                                (assoc docs k doc)))
                            {} (spark/page-docs page)))

        context (merge context docs)

        update-context (-> spa :update-page-context)
        context        (u/safe-apply update-context [context])
        update-context (-> page :update-context)
        context        (u/safe-apply update-context [context])]
    [docs context]))

(defnc DefaultAuthenticationRequired [{:keys [spa]}]
  (stack-3
   {:margin-top "30vh"}
   ;; (data (-> spa keys))
   (center
    (div
     {:font-size "120%"
      ;; :font-weight 900
      :color     "#666"}
     (local/text :authentication-required)))
   (center
    (flex
     ($ Button
        {:text     (local/text :sign-in)
         :icon     "login"
         :on-click auth/sign-in})))))

(defnc PageWrapper [{:keys [spa page children]}]
  {:helix/features {:check-invalid-hooks-usage false}}
  ;; (log ::render-PageWrapper)
  (let [context      (use-spark-context)
        params       (use-params-2)
        uid          (use-uid)
        dev-sidebar? (use-dev-sidebar-enabled)

        force-sign-in? (-> page :force-sign-in)

        sign-in-request? (and force-sign-in? (nil? uid))

        [docs context] (if sign-in-request?
                         [nil context]
                         (load-docs+context context spa page params))
        history        (use-history)]

    (reset! HISTORY history)

    (provider
     {:context SPARK_CONTEXT :value context}
     (provider ;; TODO deprecated, kill it
      {:context PAGE :value page}
      (<>
       (if sign-in-request?
         (if-let [component (-> spa :sign-in-request-component)]
           ($ component)
           ($ DefaultAuthenticationRequired {:spa spa}))
         ($ ValuesLoadGuard {:values (mapv #(get context %) (-> page :wait-for))
                             :message "Lade Seitendaten"
                             :height "95vh"
                             :full-page true}
            ($ ValuesLoadGuard
               {:values (vals docs)
                :height "95vh"
                :message "Lade Seitendaten"}
               (if dev-sidebar?
                 ($ DevSidebarPageWrapper
                    {:page    page
                     :context context}
                    children)
                 children))))
       ($ DialogsContainer)
       ($ ErrorDialog)
       ($ FormDialogsContainer))))))

(defnc PageSwitch [{:keys [spa children]}]
  ($ router/Switch
     (for [page (pages/pages-in-router-order)]
       (let [path (-> page :router-path)]
         ($ router/Route
            {:key  path
             :path path}
            ($ PageWrapper {:spa  spa
                            :page page}
               children))))))

(defnc VersionInfo []
  ($ :div
     {:style {:margin-top   "4rem"
              :margin-right "1rem"
              :text-align   :right
              :color        "lightgrey"
              :font-size    "75%"}}
     "v"
     (str (resource/inline "../spa/version.txt"))
     " · "
     (str (resource/inline "../spa/version-time.txt"))))

(def use-auth-status-message (atom-hook auth/AUTH_STATUS_MESSAGE))

(defnc AuthStatusMessage []
  (let [message (use-auth-status-message)]
    ($ :span
       message)))

(defnc AuthCompletedGuard [{:keys [children]}]
  (let [auth-completed (use-auth-completed)]
    ($ ValueLoadGuard {:value auth-completed
                       :message ($ :span
                                   "Authentifizierung: "
                                   ($ AuthStatusMessage))
                       :height "95vh"}
       children)))

(def-ui-showcase ::stack
  (stack
   (stack (tdiv-red) (tdiv-blue) (tdiv-green))
   (stack-0 (tdiv-red) (tdiv-blue) (tdiv-green))
   (stack-1 (tdiv-red) (tdiv-blue) (tdiv-green))
   (stack-2 (tdiv-red) (tdiv-blue) (tdiv-green))))

(def-ui-showcase ::flex
  (stack
   (flex (tdiv-red) (tdiv-blue) (tdiv-green))
   (flex-0 (tdiv-red) (tdiv-blue) (tdiv-green))
   (flex-1 (tdiv-red) (tdiv-blue) (tdiv-green))
   (flex-2 (tdiv-red) (tdiv-blue) (tdiv-green))))

(def-ui-showcase ::center
  (stack
   (center {:width 200 :height 200}
           ":-)")
   (div {:width 200}
        (center ":-)"))))

(def-ui-showcase ::map$
  (stack
   (map$ Button :text (range 6))
   (->> (range 6)
        (map$ Button :text)
        flex)
   (->> (range 6)
        (map$ Button :text)
        (grid ["1fr" "2fr" "1fr"]))))

(defnc AppFrame-inner [{:keys [children spa background-color]}]
  (let [class (use-app-styles-class)]
    (reset! form-ui/DIALOG-CLASS class)
    (div {:id     "AppFrame.inner"
          :background-color background-color
          :class  class
          :height "100%"}
         ($ AuthCompletedGuard
            ($ PageSwitch {:spa spa}
               children)))))

(defnc AppFrame [{:keys [children]}]
  ;; (log ::render-AppFrame)
  (let [spa                (use-spa)
        uid                (use-uid)
        spark-context      {:spark/spa  spa
                            :spark/page :MISSING!
                            :uid        uid}
        update-app-context (-> spa :update-app-context)
        spark-context      (u/safe-apply update-app-context [spark-context])]
    ($ kui/AppWrapper
       {:theme (-> spa :theme)}
       (provider
        {:context SPARK_CONTEXT
         :value   spark-context}
        ($ AppFrame-inner {:spa spa}
           children)))))

(defn load-spa [spa]
  (log ::load-spa
       :spa spa)
  (let [spa (-> spa
                (update :styles styles/adapt-styles))]
    (reset! SPA spa)
    (kui/mount ($ (-> spa :root-component)) "app")))

(defn current-version []
  (str/trim (str (resource/inline "spa/version.txt"))))

(defn current-version-time []
  (str/trim (str (resource/inline "spa/version-time.txt"))))

(defn last-upgrade-millis []
  (when-let [last-upgrade-millis-s (js/localStorage.getItem "spark.upgrade-time-millis")]
    (js/parseInt last-upgrade-millis-s)))

(defn use-reload-if-new-version-available []
  (use-effect
   :once
   (p/let [current-version (current-version)
           version-info (server-cmd> :version-info {})
           available-version (-> version-info :version)
           upgrade-available? (when available-version
                                (not= available-version current-version))]
     ;; (js/alert (str current-version " -> " available-version))
     (when upgrade-available?
       (let [current-millis (-> (js/Date.) .getTime)
             last-upgrade-millis (last-upgrade-millis)
             blocked? (and last-upgrade-millis
                           (-> current-millis (- last-upgrade-millis) (< u/millis-in-minute)))]
         (if blocked?
           (log ::upgrade-blocked
                :current-millis current-millis
                :last-upgrade-millis last-upgrade-millis)
           (do
             (log ::upgrade!
                  :current-millis current-millis
                  :last-upgrade-millis last-upgrade-millis)
             (js/localStorage.setItem "spark.upgrade-time-millis" current-millis)
             (js/window.location.reload true))))))

   nil))

(def-ui UpgradeRequest [available-version info-text reload-text color]
  (let [current-version    (current-version)
        upgrade-available? (when available-version
                             (not= available-version current-version))]
    (when upgrade-available?
      (stack-3
       {:padding 64}
       (div
        {:text-align :center}
        (or info-text "A new version is available")
        (ui/div
         {:font-size 10}
         current-version " -> " available-version))
       (center
        ($ Button
           {:on-click #(js/window.location.reload)
            :text     (or reload-text "Reload now")
            :color    (or color "secondary")}))))))

;; * misc dialogs

(defnc SelectionList [{:keys [items on-select button-class]}]
  (grid
   [:auto]
   (for [[idx item] (map-indexed vector items)]
     ($ Button
        {:key     (or (-> item :id)
                      (-> item :value)
                      idx)
         :text (local/textc (or (-> item :label)
                                (-> item :id)))
         :color (-> item :button-color)
         :on-click #(on-select item)
         :class button-class}))))

(defn show-selection-list-dialog [dialog]
  (let [dialog-id     (str "selection-list_" (u/nano-id))
        items (->> dialog :items
                   (remove nil?))
        on-select (fn [item]
                    (hide-dialog dialog-id)
                    ((or (-> item :on-select)
                         (-> dialog :on-select)) item))]
    (if (and (-> items count (= 1))
             (not (-> dialog :no-auto-select)))
      (u/later> 1 #(on-select (first items)))
      (let [SelectionList ($ SelectionList
                             {:items     items
                              :on-select on-select
                              :button-class (-> dialog :button-class)})
            dialog        (assoc dialog
                                 :id dialog-id
                                 :content (stack
                                           (-> dialog :north-content)
                                           SelectionList
                                           (-> dialog :south-content)))]
        (show-dialog dialog)))))

(defnc Confirmation [{:keys [text content
                             confirmation-text cancel-button-text
                             on-confirm on-cancel]}]
  (let [hide-dialog (use-hide-dialog)
        confirm (fn []
                  (p/let [_ (on-confirm)]
                    (hide-dialog)))]
    (stack-2
     (div
      {:text-align :center
       :font-weight :bold}
      (local/textc text))
     content
     (center
      (flex
       ($ Button
          {:text (or cancel-button-text
                     (local/text :cancel))
           :variant :text
           :on-click (fn []
                       (p/let [_ (when on-cancel (on-cancel))]
                         (hide-dialog)))})
       ($ Button
          {:text (or confirmation-text
                     (local/text :ok))
           :on-click confirm}))))))

(defn show-confirmation-dialog> [dialog]
  (let [dialog-id     (str "selection-list_" (u/nano-id))
        dialog        (-> dialog
                          (assoc :id dialog-id
                                 :content ($ Confirmation
                                             {:text (-> dialog :text)
                                              :content (-> dialog :content)
                                              :confirmation-text (-> dialog :confirmation-text)
                                              :on-confirm (-> dialog :on-confirm)
                                              :cancel-button-text (-> dialog :cancel-button-text)
                                              :on-cancel (-> dialog :on-cancel)}))
                          (dissoc :on-cancel))]
    (show-dialog> dialog)))

(defn show-confirmation-dialog [dialog]
  (show-confirmation-dialog> dialog)
  nil)

;;;
;;; storage
;;;

(defnc HiddenStorageUploadField
  [{:keys [id accept capture
           storage-path append-filename
           then
           on-upload-started
           new-file-metadata new-file-cache-hours new-file-cache-days]}]
  ($ :input
     {:id       id
      :type     "file"
      :accept   accept
      :capture  capture
      :onChange (fn [event]
                  (when-let [^js file (-> event .-target .-files (aget 0))]
                    (let [storage-path (if append-filename
                                         (conj storage-path
                                               (str (u/nano-id)
                                                    "."
                                                    (-> file .-name)))
                                         storage-path)
                          metadata new-file-metadata
                          cache-seconds (or (when new-file-cache-hours
                                              (-> new-file-cache-hours (* 3600)))
                                            (when new-file-cache-days
                                              (-> new-file-cache-days (* 86400))))
                          metadata (if cache-seconds
                                     (assoc metadata :cacheControl (str "public,max-age=" cache-seconds))
                                     metadata)]
                      (when on-upload-started
                        (on-upload-started file))
                      (-> (storage/upload-file> file storage-path metadata)
                          (.then #(storage/url> storage-path))
                          (.then then)))))
      :style    {:display "none"}}))

(defnc StorageImg [{:keys [path height width style class]}]
  (let [url (use-storage-url path)]
    ($ :img
       {:src       url
        :height    height
        :width     width
        :className class
        :style     style})))

(defnc StorageImgDiv [{:keys [path height width padding-bottom background-size class]}]
  ;; (js/console.log "DEBUG render StorageImgDiv" path padding-bottom class)
  (let [url (use-storage-url path)]
    (imgdiv url {:height height
                 :width width
                 :padding-bottom padding-bottom
                 :class          class
                 :background-size (or background-size
                                      "cover")})))

(defnc StorageImageActionArea
  [{:keys [id storage-path url
           upload-text upload-div-class
           img-style div-style
           on-url-changed
           label
           alt-url
           change-event-when-loaded-url-differs-from-alt-url?
           new-file-metadata new-file-cache-hours new-file-cache-days
           children]}]
  (let [[id _set-id] (ui/use-state (or id (str "bild_" (u/nano-id))))
        [storage-url set-storage-url_] (use-state :loading)
        set-url        (fn [new-url]
                         (when (not= storage-url new-url)
                           (log ::file-uploaded
                                :url new-url
                                :on-url-changed on-url-changed)
                           (when on-url-changed
                             (on-url-changed new-url))
                           (set-storage-url_ new-url)))
        open-file-selector #(-> (js/document.getElementById id)
                                .click)
        delete-file #(u/=> (storage/delete> storage-path)
                           (fn [_]
                             (set-url nil)))
        on-click #(if (and storage-url (not= storage-url :loading))
                    (show-confirmation-dialog {:text (local/text :delete-image?)
                                               :confirmation-text (local/text :delete)
                                               :on-confirm delete-file})
                    (open-file-selector))]

    (use-effect
      :always
      (when-not url
        (p/let [loaded-url (storage/url> storage-path)]
          (set-storage-url_ loaded-url)
          (when (and loaded-url
                     on-url-changed
                     change-event-when-loaded-url-differs-from-alt-url?
                     (not= loaded-url alt-url))
            (on-url-changed loaded-url))))
      nil)

    ($ mui/CardActionArea
       {:onClick on-click}
       ($ mui/CardContent
          (div
           ($ HiddenStorageUploadField
              {:id           id
               :accept       "image/*"
               :storage-path storage-path
               :then         set-url
               :new-file-metadata new-file-metadata
               :new-file-cache-hours new-file-cache-hours
               :new-file-cache-days new-file-cache-days})
           (stack
            (when label
              ($ FieldLabel {:text label}))

            (if (and (not url)
                     (= :loading storage-url))
              (center ($ mui/CircularProgress))
              (if-let [url (or url storage-url alt-url)]
                (cond
                  div-style ($ :div
                               {:style (merge
                                        {:background-image    (str "url(" url ")")
                                         :background-repeat   "no-repeat"
                                         :background-position "center"
                                         :background-size     "cover"}
                                        div-style)})
                  img-style (center
                             ($ :img
                                {:src   url
                                 :style img-style}))
                  :else ($ mui/Avatar
                           {:src url}))
                (div
                 {:class (str "MuiButtonBase-root MuiButton-root MuiButton-contained "
                              upload-div-class)}
                 (local/textc (or upload-text {:de "Bild auswählen..."
                                               :en "Selct picture..."})))))
            children))))))

(def-ui StorageFileButton [path idx text texts edit-options]
  (let [url (use-storage-url path)
        open-on-click (fn []
                        (js/window.open url "_blank"))
        options (into
                 [{:text "Anzeigen"
                   :on-click open-on-click}]
                 (->> edit-options
                      (map (fn [edit-option]
                             (assoc edit-option
                                    :on-click (fn []
                                                ((-> edit-option :on-click) path)))))))
        on-click (if (seq edit-options)
                   #(show-message-dialog
                     nil
                     {:options options})
                   open-on-click)]
    (if-not url
      ($ mui/CircularProgress)
      (let [text (or (get texts idx)
                     (str (or text "Datei") " " (inc idx)))]
        ($ Button
           {:key url
            :text text
            :color :secondary
             ;; :href url
             ;; :target "_blank"
            :on-click on-click})))))

(def-ui StorageFilesButtons [paths text texts edit-options]
  (flex
   (for [[idx path] (map-indexed vector paths)]
     ($ StorageFileButton
        {:key path
         :path path
         :idx idx
         :text text
         :texts texts
         :edit-options edit-options}))))

(defnc StorageFilesUploader
  [{:keys [id storage-path bucket-name upload-text
           label
           on-change
           read-only
           max-files
           on-uploaded
           on-deleted
           new-file-metadata new-file-cache-hours new-file-cache-days
           ]}]
  (let [id (or id (u/nano-id))
        max-files (or max-files 99)
        [storage-files reload-storage-files] (use-storage-files bucket-name storage-path)
        open-file-selector #(-> (js/document.getElementById id)
                                .click)
        [uploading? set-uploading] (use-state false)
        on-upload-started (fn [file]
                            (log ::StorageFilesUploader--on-upload-started
                                 :file file)
                            (set-uploading true))
        on-file-uploaded (fn [file-url]
                           (log ::StorageFilesUploader--on-uploaded
                                :file-url file-url)
                           (reload-storage-files)
                           (when on-change (on-change))
                           (set-uploading false)
                           (when on-uploaded (on-uploaded file-url))
                           )]

    (stack
     (when label
       ($ FieldLabel {:text label}))
     (div
      ($ HiddenStorageUploadField
         {:id           id
          ;; :accept       "image/jpeg"
          :storage-path storage-path
          :bucket-name bucket-name
          :append-filename true
          :on-upload-started on-upload-started
          :then         on-file-uploaded
          :new-file-metadata new-file-metadata
          :new-file-cache-hours new-file-cache-hours
          :new-file-cache-days new-file-cache-days})
      (if (or uploading?
              (not storage-files))
        ($ mui/CircularProgress)
        (stack
         ($ StorageFilesButtons
            {:paths storage-files
             :edit-options [{:text (local/text :delete)
                             :on-click (fn [path]
                                         (u/=> (storage/delete> path)
                                               (fn []
                                                 (when on-change (on-change))
                                                 (when on-deleted (on-deleted path))
                                                 (js/setTimeout
                                                  #(reload-storage-files)
                                                  500))))}]})
         (when (and storage-files
                    (not read-only)
                    (-> storage-files count (< max-files)))
           (flex
            ($ Button
               {:text (or upload-text "Datei hinzufügen...")
                :on-click open-file-selector
                :color :default}))))))
     ;; (DEBUG storage-files)
     )))

;; * db dialogs

(defn show-entity-form-dialog> [entity fields event on-changed
                                extra-buttons]
  (log ::show-entity-form-dialog>
       :entity entity
       :fields fields)
  (show-form-dialog>
   {:fields fields
    :values (select-keys entity
                         (map (fn [field]
                                (if (map? field)
                                  (-> field :id)
                                  (spark/field-schema-field-id field)))
                              fields))
    :submit (fn [values]
              (when values
                (p/let [changes (if-not event
                                  values
                                  (let [event-id (or (-> event :id)
                                                     (db/new-id))
                                        event (assoc event
                                                     :id event-id
                                                     :ts :db/timestamp
                                                     :values values
                                                     :values-before (select-keys entity (keys values)))]
                                    (assoc values :events {event-id event})))
                        _ (db-update> entity changes)]
                  (when on-changed (on-changed values)))))
    :extra-buttons extra-buttons}))


;;; Cards

(def-ui LinedCard [children class sx]
  ($ mui/Card
     {:className (str  "LinedCard" " " class)
      :sx (->sx sx)}
     children))

(defnc CardContent [{:keys [to on-click href target
                            sx sx-ActionArea class children]}]
  (cond

    on-click
    ($ mui/CardActionArea
       {:onClick on-click
        :className class
        :sx (->sx sx-ActionArea)}
       ($ mui/CardContent
          {:sx (->sx sx)}
          children))

    to
    ($ mui/CardActionArea
       {:to (coerce-link-to to)
        :component router/Link
        :className class
        :sx (->sx sx-ActionArea)
        }
       ($ mui/CardContent
          {:sx (->sx sx)}
          children))

    href
    ($ mui/CardActionArea
       {:href href
        :target target
        :className class
        :sx (->sx sx-ActionArea)}
       ($ mui/CardContent
          {:sx (->sx sx)}
          children))

    :else
    ($ mui/CardContent
       {:className class
        :sx (->sx sx)}
       children)))

(defnc FieldCardContent [{:keys [label description on-click children]}]
  ($ CardContent
     {:on-click on-click}
     ($ Field {:label label
               :description description}
        children)))

(defnc Card [{:keys [to on-click
                     children padding highlight
                     class
                     sx]}]
  (let [Card     ($ mui/Paper
                    {:className (str (when highlight "HighlightOutline")
                                     (when class
                                       (str " " class)))
                     :sx (->sx sx)}
                    (div
                     {:padding (or padding 16)
                      :display :grid}
                     children))]
    (if (or to on-click)
      ($ Link--no-styles
         {:to       to
          :on-click on-click}
         Card)
      Card)))

;;; db components

(defnc EntityFieldCardActionArea [{:keys [on-click
                                          label children
                                          entity field
                                          event
                                          value-suffix display
                                          description
                                          on-changed
                                          disabled
                                          default-value]}]
  (let [field (cond
                (map? field) field
                (spark/field-schema? field) (get field 1))
        label           (or label
                            (-> field :label))
        description (or description
                        (-> field :description))
        field-id        (-> field :id)
        value-suffix (or value-suffix
                         (-> field :value-suffix))
        value           (or (when field-id (get entity field-id))
                            default-value)
        value-component (if-let [display (or display
                                             (-> field :display))]
                          (display value)
                          (div
                           {:white-space :pre-wrap
                            :word-break :break-word}
                           (cond

                             (and (nil? value)
                                  (-> field :input-example))
                             (span
                              {:color "#bbb"}
                              (span
                               {:font-weight :normal}
                               "Beispiel: ")
                              (-> field :input-example)
                              (when-let [suffix (or (-> field :value-suffix)
                                                    (when (-> field :type (= :eur))
                                                      "€"))]
                                (str " " suffix)))

                             (and (-> field :type (= :select))
                                  (-> field :options seq))
                             (let [options (-> field :options)
                                   option (->> options
                                               (filter #(-> % :value (= value)))
                                               first)]
                               (or (when-let [mapper (-> field :option-mapper)]
                                     (mapper value))
                                   (-> option :label)
                                   (str value)))

                             (and (-> field :type (= :checkboxes))
                                  (-> field :options seq))
                             (let [options (-> field :options)
                                   options-by-value (->> options
                                                         (reduce (fn [m option]
                                                                   (assoc m
                                                                          (-> option :value)
                                                                          option))
                                                                 {}))]
                               (->> value
                                    (map (fn [option-value]
                                           (or (get-in options-by-value [option-value :label])
                                               (str option-value))))
                                    sort
                                    (str/join ", ")))

                             (and (-> field :type (= :checkboxes))
                                  (-> field :keytable))
                             (->> value
                                  sort
                                  (map (fn [option-value]
                                         (str (or (-> field :keytable (get option-value) :label)
                                                  option-value))))
                                  (str/join ", "))

                             (-> field :keytable)
                             (str (-> field :keytable (get value) :label))

                             (-> field :type (= :eur))
                             (local/format-eur value)

                             (-> field :type (= :date))
                             (local/format-date value)

                             (or (set? value)
                                 (vector? value)
                                 (list? value))
                             (str/join ", " value)

                             :else (str value))
                           (when (and value value-suffix)
                             (str " " value-suffix))))
        on-click        (or on-click
                            (when-not disabled
                              #(show-entity-form-dialog>
                                entity [field]
                                event
                                (when on-changed
                                  (fn [values]
                                    (on-changed (get values field-id))))
                                nil)))
        Field ($ FieldCardContent
                 {:label label
                  :description description}
                 (ui/div
                  {:font-size "16px"}
                  (if (and children
                           (sequential? children)
                           (seq children))
                    (<>
                       (div value-component)
                       (div children))
                    (<> value-component children))))]
    (if on-click
      ($ mui/CardActionArea
         {:onClick on-click}
         Field)
      Field)))

(defnc EntityFieldsCardActionAreas [{:keys [entity fields]}]
  (<>
   (for [field fields]
     ($ EntityFieldCardActionArea
        {:key    (-> field spark/field-schema-field-id)
         :entity entity
         :field  field}))))

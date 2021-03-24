(ns spark.ui
  (:require-macros [spark.ui :refer [<> def-ui def-ui-test
                                     map$
                                     div center
                                     grid grid-0 grid-1 grid-2
                                     grid-3 grid-4 grid-5
                                     stack stack-0 stack-1 stack-2
                                     stack-3 stack-4 stack-5
                                     flex flex-0 flex-1 flex-2
                                     flex-3 flex-4 flex-5]]
                   [spark.react :refer [use-state use-effect defnc $ provider
                                        use-context create-context]]
                   [clojure.string :as str])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]
   [shadow.resource :as resource]
   [cljs-bean.core :as cljs-bean]
   [camel-snake-kebab.core :as csk]
   [helix.core]
   [helix.dom :as d]

   ["react" :as react]
   ["react-router-dom" :as router]

   ["@material-ui/core" :as mui]
   ["@material-ui/core/styles" :as mui-styles]


   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.react :as spark-react]
   [spark.core :as spark]
   [spark.firestore :as firestore]
    
   [spark.firebase-storage :as storage]
   [spark.runtime :as runtime]
   [spark.form-ui :as form-ui]
   [spark.auth :as auth]

   [spark.firestore-hooks :as firestore-hooks]
   ))


(def atom-hook spark-react/atom-hook)
(def memo spark-react/memo)


(def StringVectorChips form-ui/StringVectorChips)
(def FormCardArea form-ui/FormCardArea)
(def FieldCardArea form-ui/FieldCardArea)
(def FieldsCardAreas form-ui/FieldsCardAreas)
(def FieldsCard form-ui/FieldsCard)
(def DocFieldCardArea form-ui/DocFieldCardArea)
(def DocFieldCard form-ui/DocFieldCard)
(def DocFieldsCard form-ui/DocFieldsCard)
(def FormDialogsContainer form-ui/FormDialogsContainer)

(def show-form-dialog form-ui/show-form-dialog)

(def Link router/Link)


(defn create-context [value]
  (-> react (.createContext value)))



;;;
;;; routing
;;;
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

;;;
;;; auth
;;;


(def use-auth-completed (atom-hook auth/AUTH_COMPLETED))

(def use-auth-user (atom-hook auth/AUTH_USER))

(defn use-uid []
  (when-let [user (use-auth-user)]
    (-> user :uid)))


;;;
;;; page and context data
;;;


(def PAGE (create-context {:page nil :data nil}))

(defn use-page []
  (use-context PAGE))


(def SPARK_CONTEXT (create-context {:spark/spa :MISSING!
                                    :spark/page :MISSING!}))

(defn use-spark-context []
  (use-context SPARK_CONTEXT))


;;;
;;; repository
;;;

(defn use-doc
  "React hook for a document."
  ([Doc doc-id]
   (spark/assert-doc-schema Doc)
   (use-doc (when doc-id
              [(spark/doc-schema-col-path Doc)
               doc-id])))
  ([path]
   (log ::use-doc
        :path path)
   (let [[doc set-doc] (use-state nil #_(when DATA @DATA))
         effect-signal (str path)
         ]

     (use-effect
      [effect-signal]
      (when path
        (log ::use-doc--subscribe
             :path path)
        ;; (set-watching true)
        (let [ref (firestore/ref path)
              on-snapshot (fn [doc-snapshot]
                            (log ::doc-snapshot-received
                                 :collection path
                                 :snapshot doc-snapshot)
                            (set-doc (firestore/wrap-doc doc-snapshot)))
              on-error (fn [^js error]
                         (log ::doc-atom-error
                              :path path
                              :exception error))
              unsubscribe (.onSnapshot ref on-snapshot on-error)]

          unsubscribe)))

     doc)))


(defn use-singleton-doc
  [Doc]
  (use-doc (spark/doc-schema-singleton-doc-path Doc)))


(defn use-col
  "React hook for a collection."
  [path]
  (log ::use-col
       :path path)
  (let [path (cond

               (spark/doc-schema? path)
               (spark/doc-schema-col-path path)

               (and (vector? path) (spark/doc-schema? (first path)))
               (into
                [(spark/doc-schema-col-path (first path))] (rest path))

               :else path)
        [docs set-docs] (use-state nil)
        effect-signal (str path)]

    (use-effect
     [effect-signal]
     (when path
       (log ::use-col--subscribe
            :path path)
       (let [col-ref (firestore/ref path)
             on-snap (fn [^js query-col-snapshot]
                       (log ::query-snapshot-received
                            :collection path
                            :count (-> query-col-snapshot .-docs count)
                            :snapshot query-col-snapshot)
                       (->> ^js query-col-snapshot
                            .-docs
                            (map firestore/wrap-doc)
                            set-docs))
             on-error (fn [^js error]
                        (js/console.error "Loading collection failed" path error))
             unsubscribe (.onSnapshot col-ref on-snap on-error)]

         unsubscribe)))

    docs))


(defn use-cols-union
  "React hook for a union of collections."
  [paths]
  (log ::use-cols-union
       :paths paths)
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
  (log ::execute-query>
       :query query
       :context context)
  (cond
    (-> query :path)
    (let [path (-> query :path (u/fn->value context))]
      (if (-> path count even?)
        (use-doc path)
        (use-col path)))

    (-> query :paths)
    (use-cols-union (-> query :paths (u/fn->value context)))

    :else
    (throw (ex-info "Invalid query. Missing :path or :paths."
                    {:query query}))))

;;;
;;; Styles / Theme
;;;


(defn use-theme []
  (mui-styles/useTheme))

(defn make-styles [styles-f]
  (mui-styles/makeStyles
   (fn [theme]
     (clj->js (styles-f theme)))))


(defn- conform-style-value [v]
  (cond
    (vector? v)(->> v (map conform-style-value) (str/join " "))
    (string? v) v
    (keyword? v) (name v)
    :else v))


(defn- conform-style [styles]
  (reduce (fn [styles [k v]]
            (if (and (string? k) (str/starts-with? "&" k))
              (assoc styles k (conform-style v))
              (assoc styles k (conform-style-value v))))
          {} styles))

(defn- conform-styles-selector [s]
  (cond
    (keyword? s) (str "& " (name s))
    (str/starts-with? s "& ") s
    :else (str "& " s)))

(defn- conform-styles [styles]
  (reduce (fn [styles [k v]]
            (assoc styles
                   (conform-styles-selector k)
                   (conform-style v)))
          {} styles))


(defn use-styles-class [styles-f]
  (when styles-f
    (let [theme (use-theme)
          styles-f-wrapper (fn [theme]
                             {:root (conform-style
                                     (if (fn? styles-f)
                                       (styles-f theme)
                                       styles-f))})
          use-styles (make-styles styles-f-wrapper)
          ^js styles (use-styles theme)]
      (-> styles .-root))))


;;;
;;; storage
;;;


(defn use-storage-files [path]
  (let [[files set-files] (use-state [])
        reload-f (fn []
                   (-> (storage/list-files> path)
                       (.then (fn [^js result]
                                (set-files (-> result .-items js->clj))))))]

    (use-effect
     :once
     (reload-f)
     nil)

    [files reload-f]))


(defn use-storage-url
  ([path]
   (use-storage-url path nil))
  ([path fallback-url]
   (let [[url set-url] (use-state nil)
         update-url (fn [url]
                      (set-url (or url
                                   fallback-url)))]

     (use-effect
      :always
      (-> (storage/url> path)
          (.then update-url)
          (.catch #(update-url nil)))
      nil)

     url)))





;;;
;;; styles
;;;

;; TODO deprecated
(defn style-bg-img [url]
  {:background-image (str "url(" url ")")
   :background-repeat "no-repeat"
   :background-position-x "center"
   :background-position-y "top"
   :background-size "contain"})




(defn reg-devcard [devcard]
  (spark/reg-test devcard))



;;;
;;; common ui functions
;;;

(defn colored-data-block [label background-color color data]
  (d/div
   {:style {:white-space "pre-wrap"
            :font-family :monospace
            :overflow "auto"
            :width "100%"
            :background-color background-color
            :color color
            :padding "1rem"
            :border-radius "4px"
            :margin "1px"
            }}
   (when label
     (div
      {:font-weight 900
       :text-align "center"}
      (with-out-str (print label))))
   (with-out-str (pprint data))))

(defn colored-data-line [label background-color color data]
  (d/div
   {:style {
            :white-space "nowrap"
            :height "50px"
            :font-family :monospace
            :width "100%"
            :background-color background-color
            :color color
            :padding "1rem"
            :border-radius "4px"
            :margin "1px"
            :display :grid
            :place-items "center"
            }}
   (div
    {:max-width "300px"
     :overflow "hidden"
     :text-overflow "ellipsis"}
    (when label
      (d/span
       {:style
        {:font-weight 900
         :text-align "center"}}
       (with-out-str (print label))
       " "))
    (with-out-str (print data)))))

(defn data [& datas]
  (d/div
   (for [[i data] (map-indexed vector datas)]
     (d/div
      {:key i}
      (colored-data-block nil "#333" "#6f6" data)))))

(defnc ExpandableData [{:keys [label value]}]
  (let [[expanded set-expanded] (use-state false)]
    (if expanded
      (colored-data-block label "#333" "#6f6" value)
      ($ :div
         {:onClick #(set-expanded true)
          :style {:cursor "pointer"}}
         (colored-data-line label "#333" "#6f6" value)))))

(defn expandable-data [label value]
  ($ ExpandableData
     {:label label
      :value value}))

(defn tdiv [color]
  ($ :div
     {:style {:background-color color
              :min-width "64px"
              :min-height "16px"}}))

(defn tdiv-red [] (tdiv "#c62828"))
(defn tdiv-blue [] (tdiv "#1565c0"))
(defn tdiv-green [] (tdiv "#2e7d32"))
(defn tdiv-yellow [] (tdiv "#f9a825"))

(def-ui-test [grid]
  (grid [:auto :auto] (tdiv-red) (tdiv-blue))
  (grid-0 [:auto :auto] (tdiv-red) (tdiv-blue))
  (grid-3 [:auto :auto] (tdiv-red) (tdiv-blue))
  (grid [:auto :auto] {:grid-gap 10} (tdiv-red) (tdiv-blue))
  (grid [:auto "200px" :auto] (tdiv-red) (tdiv-yellow) (tdiv-blue))
  (div
   {:width "200px"}
   (grid ["repeat(auto-fit, minmax(64px, 1fr))"]
         (tdiv-red) (tdiv-yellow) (tdiv-blue) (tdiv-green)
         (tdiv-red) (tdiv-yellow) (tdiv-blue) (tdiv-green))))

(defn icon [icon-name]
  (d/div
   {:class "material-icons"}
   icon-name))

(defn unsplash [width id]
  (str "https://images.unsplash.com/photo-" id "?&w=" width))


;;;
;;; def-cmp
;;;

(def-ui-test [def-ui]
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
                               (str greeting " " uid "!")))))))

;;;
;;; common components
;;;

(defnc Spacer [{:keys [width height]}]
  (let [theme (mui-styles/useTheme)]
    (d/div
     {:style {:width (-> theme (.spacing (or width 1)))
              :height(-> theme (.spacing (or width 1)))}})))


(defnc Icon [{:keys [name theme]}]
  ($ :div
     {:class (str "material-icons"
                  (when theme (str "-" theme)))}
     name))


(defnc ValueLoadGuard [{:keys [children value padding]}]
  (let [theme (mui-styles/useTheme)]
    (if value
      children
      ($ :div
          {:style {:display :flex
                   :padding (when padding (-> theme (.spacing padding)))
                   :justify-content "space-around"}}
          ($ mui/CircularProgress)))))

(defnc ValuesLoadGuard [{:keys [children values padding]}]
  (let [theme (mui-styles/useTheme)]
    (if (reduce (fn [ret value]
                  (and ret value))
                true values)
      children
      ($ :div
          {:style {:display :flex
                   :padding (when padding (-> theme (.spacing padding)))
                   :justify-content "space-around"}}
          ($ mui/CircularProgress)
          (when ^boolean goog.DEBUG
            (data values))))))


(defnc Stack [{:keys [children spacing]}]
  (let [theme (mui-styles/useTheme)]
    (d/div
     {:style {:display :grid
              :grid-gap (-> theme (.spacing (or spacing 1)))}}
     children)))


(defnc Flexbox [{:keys [children spacing style]}]
  (let [children (if (seqable? children)
                   (->> children (remove nil?))
                   [children])
        theme (mui-styles/useTheme)]
    (d/div
     {:style {:display :flex
              ;; FIXME :gap (-> theme (.spacing (or spacing 1)))
              }}
     (for [[idx child] (map-indexed vector children)]
       (d/div
        {:key idx
         :style (merge  {:margin-right (-> theme (.spacing (or spacing 1)))}
                        style)}
        child)))))

;;; dialogs

(defonce DIALOGS (atom {}))

(def use-dialogs (atom-hook DIALOGS))

(defn show-dialog [dialog]
  (let [id (or (-> dialog :id)
               (str "dialog_" random-uuid))
        dialog (assoc dialog :id id)]
    (swap! DIALOGS assoc id (assoc dialog
                                   :id id
                                   :open? true))
    id))

(defn hide-dialog [id]
  (swap! DIALOGS assoc-in [id :open?] false)
  (js/setTimeout #(swap! DIALOGS dissoc id)
                 1000)
  )

(defonce DIALOG_ID (create-context nil))

(defn use-dialog-id []
  (use-context DIALOG_ID))

(defn use-hide-dialog []
  (let [dialog-id (use-dialog-id)]
    #(hide-dialog dialog-id)))

(defnc Dialog [{:keys [dialog]}]
  {:wrap [memo]}
  (provider
   {:context DIALOG_ID
    :value (-> dialog :id)}
   ($ mui/Dialog
      {
       :open (-> dialog :open?)
       :onClose #(hide-dialog (-> dialog :id))}
      (when-let [title (-> dialog :title)]
        ($ mui/DialogTitle title))
      ($ mui/DialogContent
         (-> dialog :content)))))

(defnc DialogsContainer []
  (let [dialogs (-> (use-dialogs) vals)]
    (<>
     (for [dialog dialogs]
       ($ Dialog {:key (-> dialog :id) :dialog dialog})))))




;;; errors

(defonce ERROR (atom nil))

(def use-error (atom-hook ERROR))

(defn show-error [error]
  (runtime/report-error error)
  (reset! ERROR error))


(defn destructure-error [error]
  (cond

    (instance? js/Error error)
    [(-> error .-message)
     (ex-data error)
     (-> error .-stack)
     (ex-cause error)]

    (string? error)
    [error]

    (-> error :message)
    [(-> error :message)
     (-> error :data)]

    :else
    [(str error)]))


(defnc ErrorInfo [{:keys [error]}]
  (let [[message dat stacktrace cause] (destructure-error error)
        theme (use-theme)]
    ($ Stack
       ($ :div
          {:style {:font-size "120%"
                   :font-weight 900
                   :color (-> theme .-palette .-primary .-main)}}
          (str message))
       (when (seq dat)
         ($ :div
            {:style {:max-height "200px"
                     :overflow "auto"}}
            (data dat)))
       (when stacktrace
         ($ :div
            {:style {:max-height "200px"
                     :overflow "auto"
                     :font-family "monospace"
                     :white-space "pre-wrap"
                     :background-color "#333"
                     :color "#6ff"
                     :padding "1rem"
                     :border-radius "4px"
                     :margin "1px"
                     }}
            stacktrace))
       (when cause
         ($ ErrorInfo {:error cause})))))

(def-ui-test [ErrorInfo]
  ($ ErrorInfo {:error "Just Text"})
  ($ ErrorInfo {:error (ex-info "Clojure Exception with Data"
                                {:with "data"
                                 :and :info})})
  ($ ErrorInfo {:error (ex-info "Clojure Exception with Cause"
                                {}
                                (ex-info "Root Cause" {}))})
  ($ ErrorInfo {:error (js/Error. "JavaScript Error")}))

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
        (apply f args)
        (catch :default error (show-error error))))))


;;; links

(defnc LinkCardActionArea [{:keys [to children]}]
  ($ mui/CardActionArea
     {:to to
      :component Link}
     children))

(defnc SimpleLinkCard [{:keys [to children]}]
  ($ mui/Card
     ($ LinkCardActionArea {:to to}
        ($ mui/CardContent
           children))))


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
   (-> (runtime/execute-command> command context)
       (.then (or then identity))
       (.catch show-error))))


(defn- new-command-on-click [command context then]
  (runtime/validate-command command)
  (let [ui-context (use-spark-context)
        form (-> command :form)]
    (if-not form
      #(execute-command> command (merge ui-context context) then)
      #(let [context (merge ui-context context)
             _ (runtime/validate-command-context command context)
             form (complete-form form context)
             submit (fn [values]
                      (execute-command>
                       command
                       (assoc context :values values)
                       then))
             form (assoc form :submit submit)]
         (show-form-dialog form)))))


(defnc CommandButton [{:keys [command context then
                              icon as-icon? icon-theme
                              variant color size
                              class styles]}]
  (let [command (u/trampoline-if command)
        onClick (wrap-in-error-handler (new-command-on-click command context then))
        variant (or variant "contained")
        color (or color
                  (when (-> command :inconspicuous?) "default")
                  "primary")
        icon (when-let [icon (or icon
                                 (-> command :icon))]
               ($ Icon {:name icon
                        :theme icon-theme}))
        styles-class (use-styles-class styles)
        classes (str/join " " [class styles-class])]
    (if as-icon?
      ($ mui/IconButton
         {:onClick onClick
          :color color
          :size size
          :className classes}
         icon)
      ($ mui/Button
         {:onClick onClick
          :variant variant
          :color color
          :startIcon icon
          :size size
          :className classes}
        (spark/cmd-label command)))))

(def-ui-test [CommandButton]
  ($ CommandButton {:command {:label "default" :f (fn [_] [])}})
  ($ CommandButton {:command {:label "inconspicuous" :f (fn [_] [])
                              :inconspicuous? true}})
  ($ CommandButton {:command {:label "with icon" :f (fn [_] [])
                              :icon "thumb_up"}})
  ($ CommandButton {:command {:label "only icon" :f (fn [_] [])
                              :icon "thumb_up"}
                    :as-icon? true})
  ($ CommandButton {:command {:label "color: secondary" :f (fn [_] [])}
                    :color "secondary"}))


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

(defnc Button [{:keys [text icon
                       onClick on-click to href target
                       variant color size
                       command
                       context
                       then
                       class
                       styles]}]
  (when command
    (log ::Button.DEPRECATED.with-command
         {:command command}))
  (let [context (merge (use-spark-context)
                       context)
        command (u/trampoline-if command)
        command (when command (-> command upgrade-legacy-command complete-command))
        text (or text (-> command :label) ":text missing")
        icon (when-let [icon (or icon (-> command :icon))]
               (if (string? icon)
                 (d/div {:class "i material-icons"} icon)
                 icon))
        on-click (or on-click onClick)
        on-click (wrap-in-error-handler
                  (or on-click
                      (-> command :onClick)
                      (when command
                        #(execute-command> command context then))))
        color (or color
                  (when (-> command :inconspicuous?) "default")
                  "primary")
        styles-class (use-styles-class styles)
        classes (str/join " " [class styles-class])]
    (if to
      ($ mui/Button
         {:to to
          :component router/Link
          :variant (or variant "contained")
          :color (or color "primary")
          :startIcon icon
          :size size
          :className classes}
         text)
      ($ mui/Button
         {:onClick on-click
          :href href
          :target target
          :variant (or variant "contained")
          :color (or color "primary")
          :startIcon icon
          :size size
          :className classes}
         text))))





(defnc IconButton [{:keys [icon onClick color size command theme className
                           then context]}]
  (let [context (merge (use-spark-context)
                       context)
        command (u/trampoline-if command)
        command (when command (-> command upgrade-legacy-command complete-command ))
        onClick (wrap-in-error-handler
                 (or onClick
                     (when command (-> command :onClick))
                     (when command #(execute-command> command context then))))
        icon (when-let [icon (or icon
                                 (-> command :icon)
                                 "play_arrow")]
               (if (string? icon)
                 (d/div {:class (str "material-icons" (when theme (str "-" theme)))}
                        icon)
                 icon))]
    ($ mui/IconButton
       {:className className
        :onClick onClick
        :color color
        :size size}
       icon)))




(defnc CardOverline [{:keys [text]}]
  ($ mui/Typography
     {:variant "overline"}
     text))

(defnc SimpleCard [{:keys [title children className]}]
  ($ mui/Card
     {:className className}
     ($ mui/CardContent
        ($ Stack
           (when title ($ CardOverline {:text title}))
           ($ Stack children)))))

(def-ui-test [SimpleCard]
  ($ SimpleCard
     {:title "example title"}
     "Example content."))

(defnc CardRow [{:keys [children]}]
  (d/div
   {:style {:display :grid
            :grid-template-columns (str "repeat(" (count children) ", auto)")}}
   children))

(def FieldLabel form-ui/FieldLabel)
(def Field form-ui/Field)

(defnc FieldCardContent [{:keys [label children]}]
  ($ mui/CardContent
     ($ Field {:label label}
        children)))

;;;
;;; desktop
;;;

(defnc PageContent []
  (let [page (use-page)]
    ($ mui/Container
        {:maxWidth (get page :max-width "sm")}
        ($ ValuesLoadGuard {:values (-> page :data vals)
                            :padding 2}
           ($ (-> page :content))))
     ))


(defonce ADDITIONAL_PAGES (atom nil))


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
    :color "#eee"}
   (dev-section
    "PAGE CONTEXT"
    (div
     (for [[k v] (->> context (sort-by first))]
       (div
        {:key k}
        (expandable-data k v)))))))

(defnc DevPageWrapper [{:keys [page context children]}]
  (div
   {:display :grid
    :grid-template-columns "1fr 1fr"}
   (div
    children)
   (page-dev-sidebar page context)))

(defnc PageWrapper [{:keys [spa page devtools-component children]}]
  {:helix/features {:check-invalid-hooks-usage false}}
  ;; (log ::render-PageWrapper)
  (let [context (use-spark-context)
        params (use-params-2)

        context (assoc context :spark/page page)
        context (merge context params)

        ;; TODO query parameters form request

        docs (reduce (fn [docs [k Doc]]
                       (let [id (get context k)
                             doc (use-doc Doc id)]
                         (assoc docs k doc)))
                     {} (-> page :use-docs))
        context (merge context docs)

        update-context (-> spa :update-page-context)
        context (u/safe-apply update-context [context])
        update-context (-> page :update-context)
        context (u/safe-apply update-context [context])]

    ($ ValuesLoadGuard
       {:values (concat (mapv #(get context %) (-> page :wait-for))
                        (vals docs))
        :padding 4}
       (provider
        {:context SPARK_CONTEXT :value context}
        (provider ;; TODO deprecated, kill it
         {:context PAGE :value page}
         (div
          (if ^boolean js/goog.DEBUG
            ($ DevPageWrapper
               {:page page
                :context context}
               children)
            children)
          ($ DialogsContainer)
          ($ ErrorDialog)
          ($ FormDialogsContainer)
          (when (and  ^boolean js/goog.DEBUG devtools-component)
            ($ devtools-component))))))))


(defnc PageSwitch [{:keys [spa devtools-component children]}]
  (let [pages (spark/spa-pages spa)
        pages (concat @ADDITIONAL_PAGES pages)
        ]
    ($ router/Switch
       (for [page pages]
         ($ router/Route
            {:key (-> page :path)
             :path (-> page :path)}
            ($ PageWrapper {:spa spa
                            :page page
                            :devtools-component devtools-component}
               children))))))


(defnc VersionInfo []
  ($ :div
   {:style {:margin-top "4rem"
            :margin-right "1rem"
            :text-align :right
            :color "lightgrey"
            :font-size "75%"}}
   "v1."
   (str (resource/inline "../spa/version.txt"))
   " · "
   (str (resource/inline "../spa/version-time.txt"))))

(defnc AuthCompletedGuard [{:keys [children padding]}]
  (let [auth-completed (use-auth-completed)]
    ($ ValueLoadGuard {:value auth-completed :padding padding}
       children)))


(defn- app-styles [styles]
  (fn [theme]
    (conform-styles
     (merge {
             :.center {:display :grid :place-items "center"}

             :.grid-0 {:display :grid}
             :.grid-1 {:display :grid :grid-gap (-> theme (.spacing 1))}
             :.grid-2 {:display :grid :grid-gap (-> theme (.spacing 2))}
             :.grid-3 {:display :grid :grid-gap (-> theme (.spacing 3))}
             :.grid-4 {:display :grid :grid-gap (-> theme (.spacing 4))}
             :.grid-5 {:display :grid :grid-gap (-> theme (.spacing 5))}

             :.stack {:display :grid :grid-gap (-> theme (.spacing 1))}
             :.stack-0 {:display :grid}
             :.stack-1 {:display :grid :grid-gap (-> theme (.spacing 1))}
             :.stack-2 {:display :grid :grid-gap (-> theme (.spacing 2))}
             :.stack-3 {:display :grid :grid-gap (-> theme (.spacing 3))}
             :.stack-4 {:display :grid :grid-gap (-> theme (.spacing 4))}
             :.stack-5 {:display :grid :grid-gap (-> theme (.spacing 5))}

             ".flex" {:display :flex :flex-wrap "wrap"
                      :margin (str "-" (-> theme (.spacing 1) (/ 2)) "px")}
             ".flex > *" {:margin (str (-> theme (.spacing 1) (/ 2)) "px")}
             ".flex-0" {:display :flex :flex-wrap "wrap"}
             ".flex-1" {:display :flex :flex-wrap "wrap"
                        :margin (str "-" (-> theme (.spacing 1) (/ 2)) "px")}
             ".flex-1 > *" {:margin (str (-> theme (.spacing 1) (/ 2)) "px")}
             ".flex-2" {:display :flex :flex-wrap "wrap"
                        :margin (str "-" (-> theme (.spacing 2) (/ 2)) "px")}
             ".flex-2 > *" {:margin (str (-> theme (.spacing 2) (/ 2)) "px")}
             ".flex-3" {:display :flex :flex-wrap "wrap"
                        :margin (str "-" (-> theme (.spacing 3) (/ 2)) "px")}
             ".flex-3 > *" {:margin (str (-> theme (.spacing 3) (/ 2)) "px")}
             ".flex-4" {:display :flex :flex-wrap "wrap"
                        :margin (str "-" (-> theme (.spacing 4) (/ 2)) "px")}
             ".flex-4 > *" {:margin (str (-> theme (.spacing 4) (/ 2)) "px")}
             ".flex-5" {:display :flex :flex-wrap "wrap"
                        :margin (str "-" (-> theme (.spacing 5) (/ 2)) "px")}
             ".flex-5 > *" {:margin (str (-> theme (.spacing 5) (/ 2)) "px")}
             }
            (styles theme)))))

(def-ui-test [stack]
  (stack (tdiv-red) (tdiv-blue) (tdiv-green))
  (stack-0 (tdiv-red) (tdiv-blue) (tdiv-green))
  (stack-1 (tdiv-red) (tdiv-blue) (tdiv-green))
  (stack-2 (tdiv-red) (tdiv-blue) (tdiv-green)))

(def-ui-test [flex]
  (flex (tdiv-red) (tdiv-blue) (tdiv-green))
  (flex-0 (tdiv-red) (tdiv-blue) (tdiv-green))
  (flex-1 (tdiv-red) (tdiv-blue) (tdiv-green))
  (flex-2 (tdiv-red) (tdiv-blue) (tdiv-green)))

(def-ui-test [center]
  (center {:width 200 :height 200}
          ":-)")
  (div {:width 200}
       (center ":-)")))

(def-ui-test [map$]
  (map$ Button :text (range 6))
  (->> (range 6)
       (map$ Button :text)
       flex)
  (->> (range 6)
       (map$ Button :text)
       (grid ["1fr" "2fr" "1fr"])))

(defnc AppFrame-inner [{:keys [children styles spa]}]
  (let [class (use-styles-class (app-styles styles))]
    ($ AuthCompletedGuard
       {:padding 4}
       ($ mui/CssBaseline)
       ($ router/BrowserRouter {}
          ($ PageSwitch {:spa spa}
             ($ :div
                {:class class}
                children)))))
  )

(defnc AppFrame [{:keys [spa children theme styles]}]
  ;; (log ::render-AppFrame)
  (let [uid (use-uid)
        spark-context {:spark/spa spa
                       :spark/page :MISSING!
                       :uid uid}
        update-app-context (-> spa :update-app-context)
        spark-context (u/safe-apply update-app-context [spark-context])]
    ($ mui/ThemeProvider
       {:theme (-> theme clj->js
                   mui-styles/createMuiTheme
                   mui-styles/responsiveFontSizes)}
       (provider
        {:context SPARK_CONTEXT
         :value spark-context}
        ($ AppFrame-inner {:styles styles
                           :spa spa
                           }
           children)))))

;;;
;;; storage
;;;

(defnc HiddenStorageUploadField
  [{:keys [id accept capture
           storage-path then]}]
  ($ :input
     {:id id
      :type "file"
      :accept accept
      :capture capture
      :onChange (fn [event]
                  (when-let [file (-> event .-target .-files (aget 0))]
                    (-> (storage/upload-file> file storage-path)
                        (.then #(storage/url> storage-path))
                        (.then then))))
      :style {:display "none"}}))


(defnc StorageImg [{:keys [path height style]}]
  (let [url (use-storage-url path)]
    ($ :img
       {:src url
        :height height
        :style style})))

(defnc StorageImageActionArea
  [{:keys [id storage-path upload-text
           img-style]}]
  (let [[url set-url] (use-state :loading)]

    (use-effect
     :always
     (-> (storage/url> storage-path)
         (.then set-url))
     nil)

    ($ mui/CardActionArea
       {:onClick #(-> (js/document.getElementById id)
                      .click)}
       ($ mui/CardContent
          ($ HiddenStorageUploadField
             {:id id
              :accept "image/jpeg"
              :storage-path storage-path
              :then set-url})
          (if (= :loading url)
            ($ mui/CircularProgress)
            (if url
              (if img-style
                ($ :img
                   {:src url
                    :style img-style})
                ($ mui/Avatar
                   {:src url}))
              ($ :div (or  upload-text
                           "Bild auswählen..."))))))))


(defnc StorageImagesScroller [{:keys [storage-path reload-on-change]}]
  (let [[bilder-files reload] (use-storage-files storage-path)
        [reload-marker set-reload-marker] (use-state reload-on-change)]
    (when (not= reload-marker reload-on-change)
      (reload)
      (set-reload-marker reload-on-change))
    ($ :div
       {:style {:overflow-x "auto"
                :reload-on-change reload-on-change}}
       ($ :div
          {:style {:display :flex
                   :gap "8px"}}
          (for [picture-ref bilder-files]
            ($ StorageImg
               {:key (-> ^js picture-ref)
                :path picture-ref
                :height "200px"}))))))

;;; dialogs


(defnc SelectionList [{:keys [items on-select]}]
  ($ mui/List
     (for [[idx item] (map-indexed vector items)]
       ($ mui/ListItem
          {:key (or (-> item :id) idx)
           :button true
           :onClick #(on-select item)}
          ($ mui/ListItemText
             {:primary (-> item :label)})))))


(defn show-selection-list-dialog [dialog]
  (let [dialog-id (str "selection-list_" (random-uuid))
        SelectionList ($ SelectionList
                         {:items (-> dialog :items)
                          :on-select (fn [item]
                                       (hide-dialog dialog-id)
                                       ((-> dialog :on-select) item))})
        dialog (assoc dialog
                      :id dialog-id
                      :content SelectionList)]
    (show-dialog dialog)))

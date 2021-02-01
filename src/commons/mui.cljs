(ns commons.mui
  (:require-macros [commons.mui]
                   [spark.react :refer [use-state use-effect defnc $ provider]]
                   [clojure.string :as str])
  (:require
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [cljs.pprint :refer [pprint]]
   [shadow.resource :as resource]

   [helix.core]
   [helix.dom :as d]

   ["react" :as react]
   ["react-router-dom" :as router]

   ["@material-ui/core" :as mui]
   ["@material-ui/core/styles" :as mui-styles]

   ["material-ui-chip-input" :default ChipInput]


   [commons.logging :refer [log]]
   [commons.utils :as u]
   [commons.models :as models]
   [commons.firebase-storage :as storage]
   [commons.runtime :as runtime]
   [commons.context :as context]
   [commons.form-ui :as form-ui]

   [commons.firestore :as firestore]
   [commons.firestore-hooks :as firestore-hooks]
   ))



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
;;; repository
;;;

(def use-doc context/use-doc)
(def use-col context/use-col)
(def use-col-subset context/use-col-subset)

;;;
;;; Styles / Theme
;;;


(defn use-theme []
  (mui-styles/useTheme))

(defn make-styles [styles-f]
  (mui-styles/makeStyles
   (fn [theme]
     (clj->js (styles-f theme)))))

(defn use-styles-class [styles-f]
  (when styles-f
    (let [theme (use-theme)
          styles-f-wrapper (fn [theme]
                             {:root (if (fn? styles-f)
                                      (styles-f theme)
                                      styles-f)})
          use-styles (make-styles styles-f-wrapper)
          ^js styles (use-styles theme)]
      (-> styles .-root))))

;;;
;;; styles
;;;

(defn style-bg-img [url]
  {:background-image (str "url(" url ")")
   :background-repeat "no-repeat"
   :background-position-x "center"
   :background-position-y "top"
   :background-size "contain"})

;;;
;;; Hooks
;;;

(def atom-hook context/atom-hook)


;;;
;;; common ui functions
;;;

(defn data [& datas]
  (d/div
   (for [[i data] (map-indexed vector datas)]
     (d/div
      {:key i
       :style {:white-space "pre-wrap"
               :font-family :monospace
               :overflow "auto"
               :width "100%"
               :background-color "#333"
               :color "#6f6"
               :padding "1rem"
               :border-radius "4px"
               :margin "1px"
               }}
      (with-out-str (pprint data))))))


(defn icon [icon-name]
  (d/div
   {:class "material-icons"}
   icon-name))

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
          ($ mui/CircularProgress)))))


(defnc Stack [{:keys [children spacing]}]
  (let [theme (mui-styles/useTheme)]
    (d/div
     {:style {:display :grid
              :grid-gap (-> theme (.spacing (or spacing 1)))}}
     children)))


(defnc Flexbox [{:keys [children spacing]}]
  (let [theme (mui-styles/useTheme)]
    (d/div
     {:style {:display :flex
              ;; FIXME :gap (-> theme (.spacing (or spacing 1)))
              }}
     (for [[idx child] (map-indexed vector (if (seqable? children)
                                             children
                                             [children]))]
       (d/div
        {:key idx
         :style {:margin-right (-> theme (.spacing (or spacing 1)))}}
        child)))))


;;; errors

(defonce ERROR (atom nil))

(def use-error (commons.context/atom-hook ERROR))

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
  (let [[message dat stack cause] (destructure-error error)
        theme (use-theme)]
    ($ Stack
       ($ :div
          {:style {:font-size "120%"
                   :font-weight 900
                   :color (-> theme .-palette .-primary .-main)}}
          message)
       (when dat
         ($ :div
            {:style {:max-height "200px"
                     :overflow "auto"}}
            (data dat)))
       (when stack
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
            stack))
       (when cause
         ($ ErrorInfo {:error cause})))))


(defnc ErrorDialog []
  (let [error (use-error)]
    ($ mui/Dialog
       {:open (-> error boolean)
        :onClose #(reset! ERROR nil)}
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


(defn- new-command-on-click [command context then]
  (runtime/validate-command command)
  (let [ui-context (context/use-context-data)
        form (-> command :form)]
    (if-not form
      #(-> (runtime/execute-command> command (merge ui-context context))
           (.then (or then identity))
           (.catch show-error))
      #(let [context (merge ui-context context)
             _ (runtime/validate-command-context command context)
             form (complete-form form context)
             submit (fn [values]
                      (-> (runtime/execute-command>
                           command
                           (assoc context :values values))
                          (.then (or then identity))
                          (.catch show-error)))
             form (assoc form :submit submit)]
         (show-form-dialog form)))))


(defnc CommandButton [{:keys [command context then
                              variant color size
                              icon as-icon? icon-theme]}]
  (let [command (u/trampoline-if command)
        onClick (wrap-in-error-handler (new-command-on-click command context then))
        variant (or variant "contained")
        color (or color
                  (when (-> command :inconspicuous?) "default")
                  "primary")
        icon (when-let [icon (or icon
                                 (-> command :icon))]
               ($ Icon {:name icon
                        :theme icon-theme}))]
    (if as-icon?
      ($ mui/IconButton
         {:onClick onClick
          :color color
          :size size}
         icon)
      ($ mui/Button
         {:onClick onClick
          :variant variant
          :color color
          :startIcon icon
          :size size}
         (models/command-label command))
      )))

(defnc CommandCardArea [{:keys [command children context then]}]
  (let [command (u/trampoline-if command)
        onClick (wrap-in-error-handler (new-command-on-click command context then))]
    ($ mui/CardActionArea
       {:onClick onClick}
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
                       onClick to href target
                       variant color size
                       command
                       context
                       then
                       class
                       styles]}]
  (when command
    (log ::Button.DEPRECATED.with-command
         {:command command}))
  (let [context (merge (context/use-context-data)
                       context)
        command (u/trampoline-if command)
        command (when command (-> command upgrade-legacy-command complete-command ))
        text (or text (-> command :label) ":text missing")
        icon (when-let [icon (or icon (-> command :icon))]
               (if (string? icon)
                 (d/div {:class "i material-icons"} icon)
                 icon))
        onClick (wrap-in-error-handler
                 (or onClick
                     (-> command :onClick)
                     #(-> (runtime/execute-command> command context)
                          (.then (or then identity))
                          (.catch show-error))))
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
         {:onClick onClick
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
  (let [context (merge (context/use-context-data)
                       context)
        command (u/trampoline-if command)
        command (when command (-> command upgrade-legacy-command complete-command ))
        onClick (wrap-in-error-handler
                 (or onClick
                     (-> command :onClick)
                     #(-> (runtime/execute-command> command context)
                          (.then (or then identity))
                          (.catch show-error))))
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
  (let [page (context/use-page)]
    ($ mui/Container
        {:maxWidth (get page :max-width "sm")}
        ($ ValuesLoadGuard {:values (-> page :data vals)
                            :padding 2}
           ($ (-> page :content))))
     ))


(defnc PageSwitch [{:keys [pages devtools-component children]}]
  ($ router/Switch
     (for [page pages]
       ($ router/Route
          {:key (-> page :path)
           :path (-> page :path)}
          (provider
           {:context context/page
            :value page}
           ($ :div
              children
              ($ ErrorDialog)
              ($ FormDialogsContainer)
              (when (and  ^boolean js/goog.DEBUG devtools-component)
                ($ devtools-component))))))))


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
  (let [auth-completed (context/use-auth-completed)]
    ($ ValueLoadGuard {:value auth-completed :padding padding}
       children)))

(defnc AppFrame [{:keys [pages children theme styles]}]
  (let [class (use-styles-class styles)]
    ($ mui/ThemeProvider
       {:theme (-> theme clj->js
                   mui-styles/createMuiTheme mui-styles/responsiveFontSizes)}
       ($ AuthCompletedGuard
          {:padding 4}
          ($ mui/CssBaseline)
          ($ router/BrowserRouter {}
             ($ PageSwitch {:pages pages}
                ($ :div
                   {:class class}
                   children)))))))

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
  (let [url (context/use-storage-url path)]
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
  (let [[bilder-files reload] (context/use-storage-files storage-path)
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



;;;
;;; auth
;;;

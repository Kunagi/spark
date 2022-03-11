(ns spark.browser
  (:require
   [promesa.core :as p]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [clojure.string :as str]))

;; * Loading Scripts

(defonce LOADED_SCRIPTS (atom #{}))

(defn load-script> [src]
  (u/promise>
   (fn [resolve _reject]
     (if (contains? @LOADED_SCRIPTS src)
       (resolve src)
       (do
         (swap! LOADED_SCRIPTS conj src)
         (log ::load-script
              :src src)
         (let [script (js/document.createElement "script")]
           (set! (.-src script) src)
           (set! (.-onreadystatechange script) #(resolve src))
           (set! (.-onload script) #(resolve src))
           (-> js/document .-head (.appendChild script))))))))

;; * URL search params

(defn url-params []
  (let [params (-> js/window.location.search js/URLSearchParams.)]
    (->> ^js params
         .keys
         (map str)
         (reduce (fn [m k]
                   (assoc m (keyword k)
                          (.get ^js params k)))
                 {}))))

(comment
  (url-params))

(defn url-param [k]
  (when k
    (-> js/window.location.search
        js/URLSearchParams.
        (.get (cond
                (keyword? k) (name k)
                (string? k) k
                :else (str k))))))

(defonce URL_PARAMS (atom (url-params)))

(let [push-state (.-pushState js/history)
      replace-state (.-replaceState js/history)]
  (set! (.-pushState js/history)
        (fn [target state title url]
          (let [push-state (.bind push-state js/history)]
            (push-state target state title url))
          (reset! URL_PARAMS (url-params))))
  (set! (.-replaceState js/history)
        (fn [state title url]
          (let [replace-state (.bind replace-state js/history)]
            (replace-state state title url)
            (reset! URL_PARAMS (url-params)))))
  (js/window.addEventListener "popstate" #(reset! URL_PARAMS (url-params))))

;; * Platforms

(def ios-platforms
  #{"iPad Simulator" "iPhone Simulator" "iPod Simulator" "iPad" "iPhone" "iPod"})

;; TODO replace with navigator.userAgentData
(defn apple? []
  (or (ios-platforms (-> js/navigator.platform))
      (js/navigator.userAgent.includes "Mac")))

;; * E-Mail

(defn email-href [to fields]
  (str "mailto:" to
       (when fields
         (str "?"
              (->> fields
                   (map (fn [[k v]]
                          (str (name k) "="
                               (js/encodeURIComponent v))))
                   (str/join "&"))))))

(comment
  (email-href "wi@koczewskilde" {:subject "Hallo Welt" :body "Hey Du"}))

(defn trigger-email [to fields]
  (-> js/window.location.href (set! (email-href to fields))))

(comment
  (trigger-email "wi@koczewski.de" {:subject "Hallo Welt" :body "Here We Go"})
  (trigger-email nil {:subject "no to" :body "Here We Go"}))

;; * Downloads

(defn initiate-text-download [filename text]
  (let [a        (js/document.createElement "a")
        uri-data (js/encodeURIComponent text)]
    (-> a .-style .-display (set! "none"))
    (-> a (.setAttribute "href", (str "data:text/plain;charset=utf-8,"
                                      uri-data)))
    (-> a (.setAttribute "download" filename))
    (js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))

(defn initiate-bloburl-download [filename blob-url]
  (let [a        (js/document.createElement "a")]
    (-> a .-style .-display (set! "none"))
    (-> a (.setAttribute "href", blob-url))
    (-> a (.setAttribute "download" filename))
    (js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))

(defn initiate-pdf-bloburl-download [filename blob-url]
  (let [a        (js/document.createElement "a")]
    (-> a .-style .-display (set! "none"))
    (-> a (.setAttribute "href", blob-url))
    (-> a (.setAttribute "type" "application/pdf"))
    (-> a (.setAttribute "download" filename))
    (-> a (.setAttribute "target" "_blank"))
    (js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))

(defn download-image-to-data-url> [image-url]
  (u/promise>
   (fn [resolve _reject]
     (let [image (js/Image.)]
       (set! (.-crossOrigin image) "anonymous")
       (set! (.-onload image)
             #(let [canvas (js/document.createElement "canvas")]
                (set! (.-width canvas) (-> image .-naturalWidth))
                (set! (.-height canvas) (-> image .-naturalHeight))
                (-> canvas
                    (.getContext "2d")
                    (.drawImage image 0 0))
                (-> canvas
                    (.toDataURL "image/png")
                    resolve)))
       (set! (.-src image) image-url)))))

(defn fetch-to-blob> [url]
  (-> (js/fetch (js/Request. url))
      (.then (fn [^js response]
               (-> response
                   .blob)))))

(defn fetch-to-file> [url file-name file-type]
  (p/let [blob (fetch-to-blob> url)
          file (js/File. [blob] file-name (clj->js {:type file-type}))]
    file))

(defn fetch-to-blob-url> [url]
  (-> (js/fetch (js/Request. url))
      (.then (fn [^js response]
               (-> response
                   .blob
                   (.then (fn [blob]
                            (js/URL.createObjectURL blob))))))))

(defn fetch-to-array-buffer> [url]
  (-> (js/fetch (js/Request. url))
      (.then (fn [^js response]
               (-> response
                   .arrayBuffer)))))
;; windows

(defn- callback-on-window-closed [^js window callback]
  (js/setTimeout (fn []
                   (if (-> window .-closed)
                     (callback window)
                     (callback-on-window-closed window callback)))
                 300))

(defn open-window> [url target]
  (u/promise>
   (fn [resolve]
     (callback-on-window-closed (js/window.open url target) resolve))))

(comment
  (js/window.open "http://koczewski.de" "_blank")
  (open-window> "http://koczewski.de" "_blank"))

;; * Misc

(defn send-text-to-url-via-img [url text]
  (let [img      (js/Image.)
        uri-data (js/encodeURIComponent text)]
    (-> img .-src (set! (str url uri-data)))))

(defn install-global-error-handler [f]
  (set! (.-onerror js/window)
        (fn [message url line column error]
          (f message url line column error)
          false)))

;; * Sound

(defn play-sound [url]
  (let [audio (js/Audio. url)]
    (-> audio .play)))

(comment
  (js/console.log "hello")
  (play-sound "/snd/nachricht.ogg"))

(defonce AUDIOS (atom {}))

(defn activate-audio [k url]
  (log ::activate-audio :k k :url url)
  (let [audio (js/Audio. url)]
    (try
      (-> audio
          .play
          (.then (fn []
                   (-> audio .pause)
                   (-> audio .-currentTime (set! 0)))))
      (swap! AUDIOS assoc k audio)
      (catch :default ex
        (log ::activate-audio--failed :k k :url url :error ex)))))

(defn play-audio [k]
  (when-let [audio (get @AUDIOS k)]
    (log ::play-audio
         :k k
         :audio audio)
    (-> ^js audio .play)))

;; (defonce PLAY_HACK (atom nil))

;; (defn play-audio-hack [k]
;;   (reset! PLAY_HACK k))

;; (defn activate-audio-hack []
;;   (js/setInterval
;;    (fn []
;;      (when-let [k @PLAY_HACK]
;;        (reset! PLAY_HACK nil)
;;        (play-audio k)))
;;    1000))

;; * Webkit

(defn webkit-post-message [message-handler-name message-body]
  (log ::webkit-post-message
       :message-handler message-handler-name
       :body message-body)
  (when (and (exists? js/webkit) (exists? js/webkit.messageHandlers))
    (when-let [message-handler (aget js/webkit.messageHandlers message-handler-name)]
      (-> ^js message-handler
          (.postMessage (clj->js message-body))))))

(comment
  (webkit-post-message "test" "hallo"))

;; online / offline

(defonce OFFLINE (atom (not js/window.navigator.onLine)))
(defonce _offline-event-listener
  (do
    (js/window.addEventListener "online" #(reset! OFFLINE false))
    (js/window.addEventListener "offline" #(reset! OFFLINE true))
    :registered))

(defonce WIDTH (atom js/window.innerWidth))
(defonce _width-event-listener
  (do
    (js/window.addEventListener "resize" (fn []
                                           (let [width js/window.innerWidth]
                                             (when (not= width @WIDTH)
                                               (reset! WIDTH width)))))
    :registered))

;; sharing

(defn share-available? []
  (-> js/navigator.share boolean))

(defn share> [data]
  (u/assert (or (-> data :url)
                (-> data :text)
                (-> data :files)))
  (if js/navigator.share
    (js/navigator.share (clj->js data))
    (u/as> (js/alert (or (-> data :text)
                         (-> data :title)
                         (-> data :url))))))

(defn share [data]
  (share> data)
  nil)

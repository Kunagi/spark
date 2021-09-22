(ns spark.browser
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]))

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
  (js/window.addEventListener "popstate" #(reset! URL_PARAMS (url-params)))
  )

;; * Platforms

(def ios-platforms
  #{"iPad Simulator" "iPhone Simulator" "iPod Simulator" "iPad" "iPhone" "iPod"})

;; TODO replace with navigator.userAgentData
(defn apple? []
  (js/console.log "hallo")
  (or (ios-platforms (-> js/navigator.platform))
      (js/navigator.userAgent.includes "Mac")
      ))

;; * Downloads

(defn initiate-text-download [filename text]
  (let [a        (js/document.createElement "a")
        uri-data (js/encodeURIComponent text)]
    (-> a .-style .-display (set! "none"))
    (-> a ( .setAttribute "href", (str "data:text/plain;charset=utf-8,"
                                       uri-data)))
    (-> a (.setAttribute "download" filename))
    ( js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))

(defn initiate-bloburl-download [filename blob-url]
  (let [a        (js/document.createElement "a")]
    (-> a .-style .-display (set! "none"))
    (-> a ( .setAttribute "href", blob-url))
    (-> a (.setAttribute "download" filename))
    ( js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))

(defn initiate-pdf-bloburl-download [filename blob-url]
  (let [a        (js/document.createElement "a")]
    (-> a .-style .-display (set! "none"))
    (-> a ( .setAttribute "href", blob-url))
    (-> a (.setAttribute "type" "application/pdf"))
    (-> a (.setAttribute "download" filename))
    ( js/document.body.appendChild a)
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
                    resolve)
                ))
       (set! (.-src image) image-url)
       ))))

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
  (play-sound "/snd/nachricht.ogg")
  )

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
        (log ::activate-audio--failed :k k :url url :error ex))))
  )

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


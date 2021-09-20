(ns spark.browser
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]))

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
            (reset! URL_PARAMS (url-params))))))

;;

(def ios-platforms
  #{"iPad Simulator" "iPhone Simulator" "iPod Simulator" "iPad" "iPhone" "iPod"})

(defn apple? []
  (js/console.log "hallo")
  (or (ios-platforms (-> js/navigator.platform))
      (js/navigator.userAgent.includes "Mac")
      ))


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


(defn send-text-to-url-via-img [url text]
  (let [img      (js/Image.)
        uri-data (js/encodeURIComponent text)]
    (-> img .-src (set! (str url uri-data)))))

(defn install-global-error-handler [f]
  (set! (.-onerror js/window)
        (fn [message url line column error]
          (f message url line column error)
          false)))

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

(defn get-url-parameter [k]
  (let [k (cond
            (keyword? k) (name k)
            :else (str k))]
    (-> js/window.location.search
        js/URLSearchParams.
        (.get k))))

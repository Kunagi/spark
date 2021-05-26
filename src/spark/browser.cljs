(ns spark.browser)

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

(ns spark.browser)

(defn initiate-text-download [filename text]
  (let [a (js/document.createElement "a")
        uri-data (js/encodeURIComponent text)]
    (-> a .-style .-display (set! "none"))
    (-> a ( .setAttribute "href", (str "data:text/plain;charset=utf-8,"
                                        uri-data)))
    (-> a (.setAttribute "download" filename))
    ( js/document.body.appendChild a)
    (-> a .click)
    (js/document.body.removeChild a)))


(defn send-text-to-url-via-img [url text]
  (let [img (js/Image.)
        uri-data (js/encodeURIComponent text)]
    (-> img .-src (set! (str url uri-data)))))

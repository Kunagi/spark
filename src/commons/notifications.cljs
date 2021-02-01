(ns commons.notifications
  (:require
   [commons.logging :refer [log]]))


(defn info []
  (if-not (exists? js/Notification)
    :not-supported
    {:permission (-> js/Notification .-permission)}))

(defn supported? []
  (exists? js/Notification))

(defn permission-granted? []
  (and (supported?)
       (= "granted" (-> js/Notification .-permission))))

(defn supported-but-not-granted? []
  (and (supported?)
       (not (permission-granted?))))


(defn request-permission [callback]
  (log ::request-permission)
  (if-not (supported?)
    (log ::notifications-not-supported)
    (-> js/Notification
        .requestPermission
        (.then callback))))


(defn auto-request-permission []
  (log ::auto-request-permission)
  (when (supported-but-not-granted?)
    (request-permission (fn []))))


(defn- show-via-service-worker> [title options]
  (js/Promise.
   (fn [resolve reject]
     (-> js/navigator
         .-serviceWorker
         .getRegistration
         (.then (fn [registration]
                  (if registration
                    (-> registration
                        (.showNotification title (clj->js options))
                        (.then resolve))
                    (reject "No registration for ServiceWorker"))))))))


(defn show> [title options]
  (js/Promise.
   (fn [resolve reject]
     (if-not (supported?)
       (reject "Notifications not supported")
       (try
         (resolve (js/Notification. title (clj->js options)))
         (catch :default ex
           (log ::show-notification-failed :exception ex)
           (-> (show-via-service-worker> title options)
               (.then resolve))))))))


(defn show-once> [identifier title options]
  (js/Promise.
   (fn [resolve reject]
     (let [localstorage-key (str "notification-once." identifier)]
       (if (-> js/window .-localStorage (.getItem localstorage-key))
         (resolve "Notification already shown")
         (-> (show> title options)
             (.then #(-> js/window
                         .-localStorage
                         (.setItem localstorage-key
                                   (-> (js/Date.) .getTime str))))
             (.then resolve)))))))

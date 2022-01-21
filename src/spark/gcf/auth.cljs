(ns spark.gcf.auth
  (:require
   ["firebase-admin" :as firebase-admin]
   [promesa.core :as p]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.gcf.cmd :refer [def-cmd]]))

;; https://firebase.google.com/docs/auth/admin/manage-users
;; https://firebase.google.com/docs/reference/js/v8/firebase.auth

(defn firebase-auth []
  (-> firebase-admin
      .auth))

(comment
  firebase-admin
  (u/tap>
   (-> (firebase-auth)
       (.getUser "123")))
  )

(defn auth-me> [{:keys [uid]}]
  (log ::auth-me>
       :uid uid)
  (-> (firebase-auth)
      (.getUser uid)
      js->clj))

(def-cmd auth-me
  {:public false
   :args {}
   :f> auth-me>})

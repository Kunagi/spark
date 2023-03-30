(ns spark.gcf.auth
  (:require
   ["firebase-admin" :as firebase-admin]
   [spark.gcf.cmd :refer [def-cmd]]
   [spark.logging :refer [log]]
   [spark.utils :as u]))

;; https://firebase.google.com/docs/auth/admin/manage-users
;; https://firebase.google.com/docs/reference/admin/node/firebase-admin.auth

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

(defn delete-user> [uid]
  (-> (firebase-auth)
      (.deleteUser uid)))

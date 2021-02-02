(ns spark.firestore-init-gcf
  (:require
   ["firebase-admin" :as admin]

   [spark.firestore :as firestore]))


(defonce initialized
  (do
    (-> admin .initializeApp)
    true))


(reset! firestore/FIRESTORE (-> admin .firestore))

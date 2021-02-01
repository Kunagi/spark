(ns commons.firestore-init-gcf
  (:require
   ["firebase-admin" :as admin]

   [commons.firestore :as firestore]))


(defonce initialized
  (do
    (-> admin .initializeApp)
    true))


(reset! firestore/FIRESTORE (-> admin .firestore))

(ns spark.firestore-init-spa
  (:require
   [spark.logging :refer [log]]
   [spark.firestore :as firestore]))


(log ::init
     :debug goog.DEBUG
     :firebase js/firebase)


(when goog.DEBUG
  (defonce initialized
    (do
      (-> js/firebase .firestore (.useEmulator "localhost" 8080))
      true)))


(reset! firestore/FIRESTORE (-> js/firebase .firestore))

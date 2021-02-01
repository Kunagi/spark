(ns commons.firestore-init-spa
  (:require
   [commons.logging :refer [log]]
   [commons.firestore :as firestore]))


(log ::init
     :debug js/goog.DEBUG
     :firebase js/firebase)


(when ^boolean js/goog.DEBUG
  (-> js/firebase .firestore (.useEmulator "localhost" 8080)))


(reset! firestore/FIRESTORE (-> js/firebase .firestore))

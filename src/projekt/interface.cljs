(ns projekt.interface
  (:require
   [projekt.issues-page :as issues-page]
   [projekt.storymap-page :as storymap-page]))


(def storymap-page storymap-page/storymap-page)

(def issues-page issues-page/issues-page)

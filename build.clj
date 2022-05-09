(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]

   [kunagi.build.api :as kb]
   [kunagi.mui.build :as mui]

   ))

;; (defn update
;;   [{:keys []}]
;;   (mui/update-package-json!)
;;   (kb/npm-reinstall!))

(defn release [{:keys []}]
  (kb/release! {})
  )

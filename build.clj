(ns build
  (:refer-clojure :exclude [update])
  (:require
   [clojure.tools.build.api :as b]

   [kunagi.build.api :as kb :refer [print-task print-done print-debug]]

   [kunagi.mui.build :as mui]
   ))

(defn update
  [{:keys []}]
  (mui/update-package-json!))

(defn release [{:keys []}]
  (kb/release-2 {:project 'kunagi-build})
  (kb/release-2 {:project 'kunagi-utils})
  (kb/release-2 {:project 'kunagi-mui})
  (kb/release-2 {:project 'spark})
  )

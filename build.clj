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
  (kb/release {:project 'kunagi-build})
  (kb/release {:project 'kunagi-utils})
  (kb/release {:project 'kunagi-mui})
  (kb/release {:project 'spark})
  )

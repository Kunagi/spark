(ns spark.build-utils
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]

   [kunagi.build.api :as kb :refer [print-task print-done print-debug]]))

(defn replace-in-file [f target replacement]
  (when (-> f io/as-file .exists)
    (let [s (slurp f)
          s (-> s (.replace target replacement))]
      (spit f s))))

(def version (-> "src/spa/version.txt" slurp .trim Integer/parseInt))
(def git-version-tag (str "v" version))

(defn assert-git-clean []
  (print-task "assert-git-clean")
  (let [{:keys [out]}
        (kb/process {:command-args ["git" "status" "-s"]
                  :out :capture})]
    (when out
      (kb/fail! "git directory dirty" out))))

(defn commit-version []
  (print-task "commit-version")
  (assert-git-clean)
  ;; (kb/process {:command-args ["git" "push"]
  ;;           :dir "spark"})
  (kb/process {:command-args ["git" "tag" git-version-tag]})
  (kb/process {:command-args ["git" "push" "origin" git-version-tag]})
  (let [next-version (inc version)
        time-string (-> (kb/process {:command-args ["date" "-Iminutes"]
                                  :out :capture})
                        :out)]
    (spit "src/spa/version.txt" next-version)
    (spit "src/spa/version-time.txt" time-string)
    (kb/process {:command-args ["git" "commit"
                             "-am" (str "[version-bump] " version " -> " next-version)]})
    (kb/process {:command-args ["git" "push"]})
    (print-done version "->" next-version)))

(defn clean []
  (print-task "clean")

  (b/delete {:path ".cpcache"})
  (b/delete {:path ".shadow-cljs"})

  (b/delete {:path "firebase/public/js/spa"})
  (b/delete {:path "node_modules"})

  (b/delete {:path "firebase/functions/*.js"})
  (b/delete {:path "firebase/functions/node_modules"}))

(defn npm-install [dir]
  (print-task "npm-install")
  (kb/process {:command-args ["npm" "install"]
            :dir dir}))

(defn release-build []
  (print-task "release-build")
  (clean)
  (npm-install ".")
  (npm-install "firebase/functions")
  (kb/process {:command-args ["clojure" "-M:shadow-cljs-release"
                           "--config-merge" (str "{:release-version \"v" version "\"}")]}))

(defn firebase-deploy [project-id functions?]
  (print-task "firebase-deploy")
  (kb/process {:command-args (concat
                           ["firebase"]
                           (when project-id
                             ["--project" project-id])
                           (when-not functions?
                             ["--except" "functions"])
                           ["deploy"])
            :dir "firebase"}))

(defn update-references-to-build-artifacts []
  (replace-in-file "firebase/public/index.html" "main.js" (str "main.v" version ".js"))
  (replace-in-file "firebase/public/index.html" "index.js" (str "index.js?v=" version))
  (replace-in-file "firebase/public/index.js" "main.js" (str "main.v" version ".js")))

(defn release
  ([]
   (release {}))
  ([{:keys [pre-deploy-hook post-deploy-hook
            firebase-project-id]
     :as opts}]
   (print-task "release")
   ;; (assert-git-clean ".")
   ;; (assert-git-clean "../spark")
   (release-build)
   (update-references-to-build-artifacts)
   (when pre-deploy-hook (pre-deploy-hook))
   (firebase-deploy firebase-project-id (get opts :firebase-functions true))
   (when post-deploy-hook (post-deploy-hook))))

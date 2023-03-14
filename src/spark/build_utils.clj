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

(def version (-> "src/spa/version.txt" slurp str/trim))
(def git-version-tag (str "v" version))

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

(defn firebase-build []
  (print-task "firebase build")
  (clean)
  (npm-install ".")
  (npm-install "firebase/functions")
  (kb/process {:command-args ["clojure" "-M:shadow-cljs-release"
                           "--config-merge" (str "{:release-version \"v" version "\"}")]}))

(defn firebase-deploy [project-id functions?]
  (print-task "firebase deploy")
  (kb/process {:command-args (concat
                           ["firebase"]
                           (when project-id
                             ["--project" project-id])
                           (when-not functions?
                             ["--except" "functions"])
                           ["deploy"
                            "--non-interactive"
                            "-f"])
            :dir "firebase"}))

(defn update-references-to-build-artifacts []
  (replace-in-file "firebase/public/index.html" "main.js" (str "main.v" version ".js"))
  (replace-in-file "firebase/public/index.html" "index.js" (str "index.js?v=" version))
  (replace-in-file "firebase/public/index.js" "main.js" (str "main.v" version ".js")))

(defn deploy
  ([{:keys [pre-deploy-hook post-deploy-hook
            firebase-project-id]
     :as opts}]
   (spit "src/gcf-tap.edn" nil)
   (firebase-build)
   (update-references-to-build-artifacts)
   (when pre-deploy-hook (pre-deploy-hook))
   (firebase-deploy firebase-project-id (get opts :firebase-functions true))
   (when post-deploy-hook (post-deploy-hook))
   (kb/process {:command-args ["git" "reset" "--hard"]})))

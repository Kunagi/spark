(ns spark.build-utils
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [clojure.tools.build.api :as b]))

(defn print-action [action & args]
  (print "[" action "] ")
  (apply println args))

(defn print-info [& args]
  (print "   ")
  (apply println args))

(defn fail! [& args]
  (throw (ex-info (->> args
                       (str/join " "))
                  {})))

(defn process [params]
  (let [ret (b/process params)]
    (when (-> ret :exit (not= 0))
      (fail! "process failed"))
    ret))

(defn replace-in-file [f target replacement]
  (when (-> f io/as-file .exists)
    (let [s (slurp f)
          s (-> s (.replace target replacement))]
      (spit f s))))

(def version (-> "src/spa/version.txt" slurp .trim Integer/parseInt))
(def git-version-tag (str "v" version))

(defn assert-git-clean []
  (print-action "assert-git-clean")
  (let [{:keys [out]}
        (process {:command-args ["git" "status" "-s"]
                  :out :capture})]
    (when out
      (fail! "git directory dirty"))))

(defn commit-version []
  (print-action "commit-version")
  (assert-git-clean)
  (process {:command-args ["git" "push"]
            :dir "spark"})
  (process {:command-args ["git" "tag" git-version-tag]})
  (process {:command-args ["git" "push" "origin" git-version-tag]})
  (let [next-version (inc version)
        time-string (-> (process {:command-args ["date" "-Iminutes"]
                                  :out :capture})
                        :out)]
    (print-info version "->" next-version)
    (spit "src/spa/version.txt" next-version)
    (spit "src/spa/version-time.txt" time-string)
    (process {:command-args ["git" "commit"
                             "-am" (str "[version-bump] " version " -> " next-version)]})
    (process {:command-args ["git" "push"]})))

(defn clean []
  (print-action "clean")

  (b/delete {:path ".cpcache"})
  (b/delete {:path ".shadow-cljs"})

  (b/delete {:path "firebase/public/js/spa"})
  (b/delete {:path "node_modules"})

  (b/delete {:path "firebase/functions/*.js"})
  (b/delete {:path "firebase/functions/node_modules"}))

(defn npm-install [dir]
  (print-action "npm-install")
  (process {:command-args ["npm" "install"]
            :dir dir}))

(defn release-build []
  (print-action "release-build")
  (clean)
  (npm-install ".")
  (npm-install "firebase/functions")
  (process {:command-args ["clojure" "-M:shadow-cljs-release"
                           "--config-merge" (str "{:release-version \"v" version "\"}")]}))

(defn firebase-deploy [project-id]
  (print-action "firebase-deploy")
  (process {:command-args (concat
                           ["firebase"]
                           (when project-id
                             ["--project" project-id])
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
            firebase-project-id]}]
   (print-action "release")
   ;; (assert-git-clean ".")
   ;; (assert-git-clean "../spark")
   (release-build)
   (update-references-to-build-artifacts)
   (when pre-deploy-hook (pre-deploy-hook))
   (firebase-deploy firebase-project-id)
   (when post-deploy-hook (post-deploy-hook))))

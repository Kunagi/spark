#!/usr/bin/env bb
(ns script
  (:require
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [clojure.java.shell :refer [sh]]
   [clojure.java.io :as io]))

;;; dependencies

(require '[babashka.deps :as deps])

(deps/add-deps '{:deps {clj-commons/fs {:mvn/version "1.5.2"}}})

(require '[me.raynes.fs :as fs])


;;; helpers


(defn fail! [error]
  (println "")
  (println "FAILED!")
  (println "")
  (println error)
  (System/exit 3))


(defn exec
  ([dir command args]
   (exec dir command args nil))
  ([dir command args more-args]
   (let [as-args (fn [args]
                   (if (string? args)
                     [args]
                     args))
         {:keys [exit out err]} (apply sh (-> [command]
                                              (into (as-args args))
                                              (into (as-args more-args))
                                              (into [:dir dir])))]
     ;; (when-not (str/blank? out)
     ;;   (println out))
     (when-not (str/blank? err)
       (println err))
     (when-not (= exit 0)
       (fail! (str "Shell Command " command " failed")))
     out)))


(defn exec-in-terminal [dir title cmd]
  (exec dir "gnome-terminal" ["-t" title "--"] cmd))


;;; config

(defn load-config []
  (-> "spark.edn" slurp read-string))

(def config (memoize load-config))


;;; clean

(defn clean! []
  (fs/delete-dir ".cpcache")
  (fs/delete-dir ".shadow-cljs")
  (fs/delete-dir "node_modules")
  (fs/delete-dir "firebase/public")
  (fs/delete-dir "firebase/public/node_modules")
  (doseq [file (fs/find-files "firebase/functions" #".*\.js")]
    (fs/delete file)))


;;; provide-resources

(defn collect-spa-resources! []
  (let [dst-dir "firebase/public"
        dst-dir-js "firebase/public/js"]
    (fs/mkdirs dst-dir-js)
    (fs/copy-dir-into "resources/spa" dst-dir)

    (let [{:keys [spa-files spa-js-files]} (config)]
      (doseq [path spa-files]
        (fs/copy path (str dst-dir "/" (-> path io/as-file .getName))))
      (doseq [path spa-js-files]
        (fs/copy path (str dst-dir-js "/" (-> path io/as-file .getName)))))))


;;; start-firebase

(defn start-firebase! []
  (collect-spa-resources!)
  (exec-in-terminal
   "firebase" "Firebase Emulators"
   ["firebase" "emulators:start" "--import=./dev-data" "--export-on-exit"]))


;;; start-shadow-watch

(defn write-version-time []
  (let [s (exec "." "date" "-Iminutes")]
    (spit "src/spa/version-time.txt" s)))

(defn start-shadow-watch! []
  (write-version-time)
  (exec-in-terminal
   "." "Shadow-CLJS watch"
   ["clojure" "-A:shadow-cljs" "watch" "spa" "gcf" "test"]))


;;; update-npm

(defn npm-install! []
  (exec "firebase/functions" "gnome-terminal" ["--wait" "-t" "NPM install" "--"] [ "npm" "install"])
  (exec "." "gnome-terminal" ["--wait" "-t" "NPM install" "--"] [ "npm" "install"]))


(defn dev! []
  (clean!)
  (npm-install!)
  (start-shadow-watch!)
  (start-firebase!))

;;; cli

(defn usage! []
  (println "Usage: <command>")
  (System/exit 1))


(let [[command] *command-line-args*]
  (if (empty? command)
    (usage!)
    (do
      (case command

        "clean" (clean!)
        "collect-spa-resources" (collect-spa-resources!)
        "start-firebase" (start-firebase!)
        "start-shadow-watch" (start-shadow-watch!)
        "npm-install" (npm-install!)
        "dev" (dev!)

        (do
          (println "Unknown spark command:" command)
          (System/exit 2)))
      nil)))

(ns spark.dev.adoc
  (:require
   [clojure.string :as str]))


(defn section [& parts]
  (let [s (str/join " " parts)]
    (when-not (str/blank? s)
      (str "\n" s "\n"))))


(defn table [config rows]
  (str "\n[" config "]\n"
       "|===\n"
       (->> rows
            (map (fn [row]
                   (->> row
                        (map (fn [cell]
                               (str "| " cell "\n")))
                        (str/join ""))))
            (str/join "\n"))
       "|===\n"))


(defn monospace [s]
  (str "`" s "`"))

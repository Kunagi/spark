(ns spark.ui.cmd-console-page
  (:require
   ["@mui/material" :as mui]
   [clojure.edn :as edn]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.ui :as ui :refer [def-ui def-page $]]

   ))

(defonce INPUT (atom "{:cmd :dummy}"))
(def use-input (ui/atom-hook INPUT))

(defonce OUTPUT (atom nil))
(def use-output (ui/atom-hook OUTPUT))

(defn execute> [data]
  (u/=> (ui/server-cmd> (-> data :cmd) (dissoc data :cmd))
        (fn [result]
          (reset! OUTPUT result))))

(def-ui Input []
  (let [input (use-input)]
    ($ mui/Card
       ($ mui/CardContent
          ($ mui/TextField
             {
              :id              "cmdData"
              :name            "cmdData"
              :value           input
              :required        true
              :error           nil
              :helperText      nil
              :onChange        #(reset! INPUT (-> % .-target .-value))
              ;; :onKeyPress      (when-not (-> field :multiline?)
              ;;                    #(when (= "Enter" (-> ^js % .-nativeEvent .-code))
              ;;                       ((:on-submit field))))
              :label           "Command"
              :multiline       true
              :rows            8
              :autoFocus       true
              :margin          "dense"
              ;; :variant         "outlined"
              :fullWidth       true
              :sx (clj->js {"& textarea" {"font-family" "monospace"
                                          "font-size" "14px"
                                          "line-height" "20px"
                                          "font-style" "normal"}})}))
       ($ mui/CardActions
          ($ ui/Button
             {:text "Execute"
              :on-click #(execute> (edn/read-string input))})))))

(def-ui Output []
  (let [output (use-output)]
    (when output
      ($ mui/Card
         ($ mui/CardContent
            (ui/data output))))))


(def-ui PageContent []
  (ui/stack
   ($ Input)
   ($ Output)))

(def-page cmd-console-page
  {:path             "/ui/cmd-console"
   :title "Command Console"
   :content          PageContent
   })

(ns spark.auth-ui
  (:require
   [clojure.string :as str]
   ["@material-ui/core" :as mui]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.repository :as repository]
   [spark.ui :as ui :refer [def-ui $]]
   [spark.auth :as auth]

   [clojure.string :as str]))

(def use-email-sign-in (ui/atom-hook auth/EMAIL_SIGN_IN))

(def-ui EmailProcess []
  (let [email-sign-in       (use-email-sign-in)
        [email set-email]   (ui/use-state (-> email-sign-in :email))
        continue-with-email (fn []
                              (when-not (str/blank? email)
                                (swap! auth/EMAIL_SIGN_IN assoc
                                       :email email
                                       :status :sending-email)
                                (auth/send-sign-in-link-to-email
                                 email (-> email-sign-in :url)))
                              )]
    ($ ui/Stack
       #_(ui/data email-sign-in)
       (when (= :waiting-for-email (-> email-sign-in :status))
         "Öffne deine E-Mail und Klicke den Link!")
       (when (= :sending-email (-> email-sign-in :status))
         "E-Mail wird versendet...")
       (when (= :input-email (-> email-sign-in :status))
         (ui/stack-3
          (ui/div
           "Wir schicken einen Link, der zur Anmeldung fürht.
Bitte E-Mail Adresse eingeben.")
          ($ :form
             {:onSubmit (fn [^js event]
                          (continue-with-email)
                          (-> event .preventDefault)
                          false)}
             ($ mui/TextField
                {:defaultValue email
                 :onChange     #(set-email (-> % .-target .-value))
                 :id           "email"
                 :name         "email"
                 :type         "email"
                 :label        "E-Mail"
                 :required     true
                 :autoFocus    true
                 :fullWidth    true
                 :variant      "outlined"}))
          (ui/flex
           {:justify-content :flex-end}
           ($ ui/Button
              {:text     "Abbrechen"
               :variant  "text"
               :on-click #(reset! auth/EMAIL_SIGN_IN nil)})
           ($ ui/Button
              {:text     "Link anfordern"
               :on-click continue-with-email}))
          )))))

(def-ui LoginSelector [email google microsoft facebook]
  (if (use-email-sign-in)
    ($ EmailProcess)
    (ui/stack
     ;; (ui/div
     ;;  "Anmeldemethode auswählen")
     (when google
       ($ ui/Button
          {:text      "Google"
           :onClick   auth/sign-in-with-google
           :startIcon :login }))
     (when microsoft
       ($ ui/Button
          {:text      "Microsoft"
           :onClick   auth/sign-in-with-microsoft
           :startIcon :login }))
     (when facebook
       ($ ui/Button
          {:text      "Facebook"
           :onClick   auth/sign-in-with-facebook
           :startIcon :login}))
     (when email
       ($ ui/Button
          {:text      "E-Mail"
           :onClick   auth/sign-in-with-email
           :startIcon :login }))
     ($ :div))))


(defn show-sign-in-selector-dialog [options]
  (log ::sign-in)
  (ui/show-dialog {:id      "sign-in"
                   :title   "Anmeldung"
                   :content ($ LoginSelector
                               {:email     (-> options :email)
                                :google    (-> options :google)
                                :microsoft (-> options :microsoft)
                                :facebook  (-> options :facebook)})}))

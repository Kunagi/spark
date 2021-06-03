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
                              )

        CancelButton ($ ui/Button
                        {:text     "Abbrechen"
                         :variant  "text"
                         :on-click #(reset! auth/EMAIL_SIGN_IN nil)})
        ]
    (ui/stack
     ;; (when goog.DEBUG (ui/data email-sign-in))

     (if-let [error (-> email-sign-in :error)]

       (ui/stack
        ($ ui/ErrorInfo
           {:error error})
        (ui/center CancelButton))

       (ui/stack

        (when (= :waiting-for-email (-> email-sign-in :status))
          (ui/stack
           (ui/div "Öffne deine E-Mail und Klicke den Link!")
           (ui/center CancelButton)))

        (when (= :sending-email (-> email-sign-in :status))
          (ui/stack
           (ui/div "E-Mail wird versendet...")
           (ui/center CancelButton)))

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
            CancelButton
            ($ ui/Button
               {:text     "Link anfordern"
                :on-click continue-with-email}))
           ))))

     )))

(defn initialize-telephone-sign-in []
  (log ::initialize-telephone-sign-in)
  (let [verifier (js/firebase.auth.RecaptchaVerifier.
                  "recaptcha-container"
                  (clj->js {:size     :invisible
                            :callback (fn [^js response]
                                        (log ::DEBUG--callback
                                             :response response))}))]
    (-> js/window .-recaptchaVerifier (set! verifier))))

(def-ui RecaptchaContainer []

  (ui/use-effect
   :once
   (initialize-telephone-sign-in)
   nil)

  (ui/div { :id "recaptcha-container"})
  )

(def-ui LoginSelector [email telephone google microsoft facebook]
  (if (use-email-sign-in)
    ($ EmailProcess)

    (ui/stack-3
     (ui/center
      (ui/div "Wie möchtest du dich anmelden?"))
     (ui/stack
      (when google
        ($ ui/Button
           {:text     "Google"
            :on-click auth/sign-in-with-google}))
      (when microsoft
        ($ ui/Button
           {:text     "Microsoft"
            :on-click auth/sign-in-with-microsoft}))
      (when facebook
        ($ ui/Button
           {:text     "Facebook"
            :on-click auth/sign-in-with-facebook}))
      (when telephone
        (ui/<>
         ($ RecaptchaContainer)
         ($ ui/Button
            {:text     "Telefon / SMS"
             :id       "telephone-sign-in-button"
             :on-click auth/sign-in-with-telephone
             ;; :on-click (fn [_]
             ;;             (initialize-telephone-sign-in))
             })))
      (when email
        ($ ui/Button
           {:text     "E-Mail"
            :on-click auth/sign-in-with-email}))
      ($ :div)))))

(defn show-sign-in-selector-dialog [options]
  (log ::sign-in)
  (ui/show-dialog {:id      "sign-in"
                   :title   "Anmelden / Registrieren"
                   :content ($ LoginSelector
                               {:email     (-> options :email)
                                :telephone (-> options :telephone)
                                :google    (-> options :google)
                                :microsoft (-> options :microsoft)
                                :facebook  (-> options :facebook)})}))

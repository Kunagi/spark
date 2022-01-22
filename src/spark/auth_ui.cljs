(ns spark.auth-ui
  (:require
   [clojure.string :as str]
   ["@material-ui/core" :as mui]
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.ui :as ui :refer [def-ui $]]
   [spark.auth :as auth]
))

(def use-auth-user (ui/atom-hook auth/AUTH_USER))
;; * E-Mail

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

;; * Telephone

(defn initialize-telephone-sign-in []
  (log ::initialize-telephone-sign-in)
  (let [verifier (js/firebase.auth.RecaptchaVerifier.
                  "recaptcha-container"
                  (clj->js {:size     :invisible
                            :callback (fn [^js response]
                                        (log ::recaptcha-verivier-initialized
                                             :response response))}))]
    (-> js/window .-recaptchaVerifier (set! verifier))))

(def use-telephone-sign-in (ui/atom-hook auth/TELEPHONE_SIGN_IN))

(def-ui RecaptchaContainer []

  (ui/use-effect
   :once
   (initialize-telephone-sign-in)
   nil)

  (ui/div { :id "recaptcha-container"})
  )



(defn coerce-telephone [s]
  (let [s (str/replace s " " "")
        s (if (-> s (.startsWith "00"))
            (str/replace s "00" "+")
            s)
        s (if (-> s (.startsWith "0"))
            (str "+49" (-> s (.substring 1)))
            s)]
    s))

(defn validate-telephone [s]
  (let [s (coerce-telephone s)]
    (when (or (-> s count (< 7))
              (not (re-matches #"\+[0-9]*" s)))
      "Ungültige Telefonnummer")))

(def-ui TelephoneProcess []
  (let [status (use-telephone-sign-in)
        [telephone set-telephone] (ui/use-state (-> status :telephone))
        [code set-code] (ui/use-state (-> status :code))
        [input-error set-input-error] (ui/use-state nil)
        continue-with-telephone (fn []
                                  (when-not (str/blank? telephone)
                                    (let [error (validate-telephone telephone)]
                                      (if error
                                        (set-input-error error)
                                        (do
                                          (set-input-error nil)
                                          (swap! auth/TELEPHONE_SIGN_IN assoc
                                                 :telephone telephone
                                                 :status :sending-sms)
                                          (auth/send-sign-in-code-to-telephone
                                           (coerce-telephone telephone)))))))

        continue-with-code (fn []
                             (when-not (str/blank? code)
                               (swap! auth/TELEPHONE_SIGN_IN assoc
                                      :code code
                                      :status :checking-code)
                               (auth/sign-in-with-telephone-code code))
                             )

        CancelButton ($ ui/Button
                        {:text     "Abbrechen"
                         :variant  "text"
                         :on-click #(reset! auth/TELEPHONE_SIGN_IN nil)})
        ]
    (ui/stack
     ($ RecaptchaContainer)
     (when goog.DEBUG (ui/data status))

     (if-let [error (-> status :error)]

       (ui/stack
        ($ ui/ErrorInfo
           {:error error})
        (ui/center CancelButton))

       (ui/stack

        (when (= :sending-sms (-> status :status))
          (ui/stack
           (ui/div "SMS wird versendet...")
           (ui/center CancelButton)))

        (when (= :input-telephone (-> status :status))
          (ui/stack-3
           (ui/div
            "Wir schicken einen Sicherheitscode per SMS.
Bitte mobile Telefonnummer eingeben.")
           ($ :form
              {:onSubmit (fn [^js event]
                           (continue-with-telephone)
                           (-> event .preventDefault)
                           false)}
              ($ mui/TextField
                 {:defaultValue telephone
                  :onChange     #(set-telephone (-> % .-target .-value))
                  :id           "telephone"
                  :name         "telephone"
                  :label        "Telefonnummer mobil"
                  :auto-complete "tel"
                  :error        (boolean input-error)
                  :helperText   input-error
                  :required     true
                  :autoFocus    true
                  :fullWidth    true
                  :variant      "outlined"}))
           (ui/flex
            {:justify-content :flex-end}
            CancelButton
            ($ ui/Button
               {:text     "SMS anfordern"
                :on-click continue-with-telephone}))
           ))

        (when (= :input-code (-> status :status))
          (ui/stack-3
           (ui/div
            "Wir haben dir einen Sicherheitscode per SMS geschickt.
Bitte gib hier den empfangenen Code ein.")
           ($ :form
              {:onSubmit (fn [^js event]
                           (continue-with-telephone)
                           (-> event .preventDefault)
                           false)}
              ($ mui/TextField
                 {:defaultValue code
                  :onChange     #(set-code (-> % .-target .-value))
                  :id           "code"
                  :name         "code"
                  :type         "number"
                  :label        "Sicherheitscode"
                  :inputProps   (clj->js {:pattern "[0-9]*"})
                  :required     true
                  :autoFocus    true
                  :fullWidth    true
                  :variant      "outlined"}))
           (ui/flex
            {:justify-content :flex-end}
            CancelButton
            ($ ui/Button
               {:text     "Anmelden"
                :on-click continue-with-code}))
           ))))

     )))



;; * Selector

(def-ui LoginSelector [email telephone google apple microsoft facebook]
  (let [email-sign-in (use-email-sign-in)
        telephone-sign-in (use-telephone-sign-in)]
    (cond
      email-sign-in
      ($ EmailProcess)

      telephone-sign-in
      ($ TelephoneProcess)

      :else
      (ui/stack-3
       (ui/center
        (ui/div "Welchen Dienst möchtest Du zur Identifizierung verwenden?"))
       (ui/stack
        (when google
          ($ ui/Button
             {:text     "Google"
              :on-click auth/sign-in-with-google}))
        (when apple
          ($ ui/Button
             {:text     "Apple"
              :on-click auth/sign-in-with-apple}))
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
           ($ ui/Button
              {:text     "Mobiltelefon / SMS"
               :id       "telephone-sign-in-button"
               :on-click auth/sign-in-with-telephone
               })))
        (when email
          ($ ui/Button
             {:text     "E-Mail"
              :on-click auth/sign-in-with-email}))
        ($ :div))))))

(defn show-sign-in-selector-dialog [options]
  (log ::show-sign-in-selector-dialog
       :options options)
  (ui/show-dialog {:id      "sign-in"
                   ;; :title   "Identifizierung"
                   :content ($ LoginSelector
                               {:email     (-> options :email)
                                :telephone (-> options :telephone)
                                :google    (-> options :google)
                                :microsoft (-> options :microsoft)
                                :apple     (-> options :apple)
                                :facebook  (-> options :facebook)})}))

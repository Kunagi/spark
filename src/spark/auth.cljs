(ns spark.auth
  (:require

   [promesa.core :as p]
   ["firebase/auth" :as firebase-auth]

   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.env-config :as env-config]
   [spark.firebase.auth :as spark.firebase.auth]

   [spark.core :as spark]
   [spark.db :as db]
   [spark.browser :as browser]
   [spark.firebase.messaging :as messaging]))

(defonce SIGN_IN-F (atom nil))
(defonce AUTH_COMPLETED (atom false))
(defonce AUTH_STATUS_MESSAGE (atom "Initialisierung"))
(defonce AUTH_USER (atom nil))
(defonce MESSAGING_TOKEN (atom nil))

(defn auth-completed? []
  @AUTH_COMPLETED)

(defn auth-user []
  @AUTH_USER)

(defn uid []
  (when-let [auth-user (auth-user)]
    (-> ^js auth-user :uid)))

(defn auth []
  (spark.firebase.auth/auth))

(defn redirect [href]
  (js/window.location.replace href))

(defn redirect-to-home []
  (redirect "/"))

(defn process-sign-in-with-custom-token-from-url [error-handler]
  (let [custom-token (browser/url-param "customAuthToken")]
    (when custom-token
      (log ::process-sign-in-with-custom-token-from-url
           :custom-token custom-token)
      (reset! AUTH_STATUS_MESSAGE (str "Custom Token empfangen: " custom-token))
      (-> (firebase-auth/signInWithCustomToken (auth) custom-token)
          (.then (fn [_]
                   (reset! AUTH_STATUS_MESSAGE "Mit Custon Token angemeldet")))
          (.catch (fn [^js error]
                    (log ::sign-in-with-redirect-failed
                         :error error)
                    (reset! AUTH_STATUS_MESSAGE (str "Fehler mit Custom Token: "
                                                     custom-token
                                                     " "
                                                     (str error)))
                    (when error-handler (error-handler error))))))))

(defn process-sign-in-with-redirect [error-handler]
  (-> (firebase-auth/getRedirectResult (auth))
      (.catch (fn [^js error]
                (log ::sign-in-with-redirect-failed
                     :error error)
                (when error-handler (error-handler error))))))

(defn process-sign-in-with-email-link [error-handler]
  (let [href js/window.location.href]
    (when (firebase-auth/isSignInWithEmailLink (auth) href)
      (log ::sign-in-with-email-link
           :href href)
      (let [email (or (js/localStorage.getItem "signInEmail")
                      (js/window.prompt "Bitte E-Mail zur Bestätigung eingeben"))]
        (-> (firebase-auth/signInWithEmailLink (auth) email href)
            (.then #(do
                      (js/localStorage.removeItem "signInEmail")
                      (redirect js/window.location.pathname))
                   #(do
                      (js/console.error %)
                      (if error-handler
                        (error-handler %)
                        (redirect-to-home)))))))))

(defn- email-domain [email]
  (when email (-> email (.substring (-> email (.indexOf "@") inc)))))

(defn update-user-doc [doc-schema auth-user update-user messaging-vapid-key]
  (log ::update-user-doc
       :doc-schema doc-schema)
  (let [uid   (-> auth-user :uid)
        email (-> auth-user :email)
        phone (-> auth-user :phone-number)
        col-path (spark/doc-schema-col-path doc-schema)
        doc-path (str col-path "/" uid)]
    (p/let [messaging-token (when messaging-vapid-key
                              (messaging/get-token> messaging-vapid-key))
            _ (when messaging-token
                (reset! MESSAGING_TOKEN messaging-token))

            user (db/get> doc-path)

            user-changes {:db/ref doc-path
                  :id uid
                  :uid uid
                  :auth-phone phone
                  :auth-email email
                  :auth-domain (email-domain email)
                  :auth-display-name (-> auth-user :display-name)
                  :auth-timestamp [:db/timestamp]
                  :auth-ts-creation (-> auth-user :ts-creation)
                  :auth-ts-last-sign-in (-> auth-user :ts-last-sign-in)}
            device (when messaging-token
                     {:id messaging-token
                      :type :web
                      :user-agent js/navigator.userAgent
                      :ts :db/timestamp
                      :zeit-anmeldung :db/timestamp
                      :disabled false
                      :error nil
                      :error-ts nil})
            user-changes (if device
                   (assoc-in user-changes [:devices (-> device :id)] device)
                   user-changes)

            _ (if (db/doc-exists? user)
                (db/update> user user-changes)
                (db/add> col-path user-changes))]
      (when update-user
        (p/let [user (db/get> doc-path)
                 user (u/update-if user update-user auth-user)
                ]
          (db/update> user user))))))

(defn- import-user [^js u]
  (log ::import-user
       :user u)
  (when u
    {:display-name (-> u .-displayName)
     :email (-> u .-email)
     :email-verified (-> u .-emailVerified)
     :anonymous (-> u .-isAnonymous)
     :phone-number (-> u .-phoneNumber)
     :photo-url (-> u .-photoURL)
     :tenant-id (-> u .-tenantId)
     :uid (-> u .-uid)
     :ts-last-sign-in (-> u .-metadata .-lastSignInTime js/Date.)
     :ts-creation (-> u .-metadata .-creationTime js/Date.)}))

(defn initialize [{:keys [user-doc-schema update-user set-user sign-in error-handler
                          messaging-vapid-key]}]
  (log ::initialize
       :doc-schema user-doc-schema)
  (reset! SIGN_IN-F sign-in)

  (reset! AUTH_STATUS_MESSAGE "Initialisierung gestartet")

  ;; (when-not (fn? (-> ^js firebase .-auth))
  ;;   (js/setTimeout
  ;;    #(js/window.location.reload)
  ;;    1000))

  (reset! AUTH_STATUS_MESSAGE "Initialisierung 2 gestartet")
  (firebase-auth/useDeviceLanguage (auth))
       ;; https://firebase.google.com/docs/reference/js/firebase.auth.Auth#onauthstatechanged
  (-> (firebase-auth/onAuthStateChanged
       (auth)
       (fn [^js google-js-user]
         (js/localStorage.setItem "spark.uid" (when google-js-user
                                                (-> google-js-user .-uid)))
         (js/console.log "AUTH" google-js-user)
         (reset! AUTH_STATUS_MESSAGE "AuthState empfangen")
         (let [user (import-user google-js-user)]
           (reset! AUTH_STATUS_MESSAGE "Benutzerdaten empfangen")
           (log ::auth-state-changed
                :user user)
           (let [auth-completed? (auth-completed?)]
             (when-not auth-completed?
               (log ::auth-completed :user user))
             (when-not (= user @AUTH_USER)
               (when auth-completed?
                 (log ::user-changed :user user)
                 (when-not user
                   (reset! AUTH_STATUS_MESSAGE "Umleitung zur Startseite")
                   (redirect-to-home)))
               (reset! AUTH_USER user)
               (when user
                 (when user-doc-schema
                   (reset! AUTH_STATUS_MESSAGE "Aktualisiere Benutzer Datensatz")
                   (update-user-doc user-doc-schema user update-user messaging-vapid-key)))
               (when set-user
                 (set-user user)))
             (when-not auth-completed?
               (reset! AUTH_STATUS_MESSAGE "abgeschlossen")
               (reset! AUTH_COMPLETED true)))))))

  (reset! AUTH_STATUS_MESSAGE "initialisierung abgeschlossen")

  (process-sign-in-with-custom-token-from-url error-handler)

  (process-sign-in-with-redirect error-handler)

  (process-sign-in-with-email-link error-handler)

  nil)

;; signInWithRedirect does not work on firefox with Enanced Tracking Protection enabled
(defn provider-sign-in> [^js provider]
  (if (browser/ios?)
    (firebase-auth/signInWithRedirect (auth) provider)
    (-> (firebase-auth/signInWithPopup (auth) provider)
        (.then #(js/window.location.reload)))))

(comment
  (js/console.log (-> (firebase-auth)))
  (js/console.log firebase-auth/GoogleAuthProvider))

(defn sign-in-with-google []
  (log ::sign-in-with-google)
  (let [provider           (firebase-auth/GoogleAuthProvider.)]
    (.addScope ^js provider "openid")
    (.addScope ^js provider "profile")
    (.addScope ^js provider "email")
    (.addScope ^js provider "https://www.googleapis.com/auth/userinfo.email")
    (.addScope ^js provider "https://www.googleapis.com/auth/userinfo.profile")
    (-> ^js provider (.setCustomParameters #js {:prompt "select_account"}))
    (provider-sign-in> provider)))

(defn sign-in-with-microsoft []
  ;; https://firebase.google.com/docs/auth/web/microsoft-oauth?authuser=0
  ;; https://docs.microsoft.com/en-us/azure/active-directory/develop/quickstart-register-app
  ;; https://docs.microsoft.com/en-us/azure/active-directory/azuread-dev/v1-protocols-oauth-code
  ;; pplication (client) ID: fc6ee63d-375e-432c-b3d9-2722eba6a0ee
  ;; Directory (tenant) ID: 088fc83d-8363-42a6-8b7f-02c6f326cb17
  ;; Object ID; 5275d8a6-c7de-4367-a67f-7955ff60c4d6
  (let [AuthProvider firebase-auth/OAuthProvider
        provider     (AuthProvider. "microsoft.com")]
    (-> ^js provider (.setCustomParameters #js {:prompt "login"}))
    (provider-sign-in> provider)))

(defn sign-in-with-apple []
  (let [AuthProvider firebase-auth/OAuthProvider
        provider     (AuthProvider. "apple.com")]
    (.addScope ^js provider "name")
    (.addScope ^js provider "email")
    (-> ^js provider (.setCustomParameters #js {:locale "de"}))
    (provider-sign-in> provider)))

(defn sign-in-with-facebook []
  ;; https://firebase.google.com/docs/auth/web/facebook-login
  ;; https://developers.facebook.com
  ;; https://developers.facebook.com/docs/permissions/reference
  (let [FacebookAuthProvider firebase-auth/FacebookAuthProvider
        provider             (FacebookAuthProvider.)]
    (-> ^js provider (.addScope "email"))
    (-> (provider-sign-in> provider)
        (.then (fn [result]
                 (js/console.log "AUTH SUCCESS" result))
               (fn [result]
                 (js/console.log "AUTH FAILURE" result))))))

(defn sign-in
  ([]
   (sign-in {}))
  ([opts]
   (let [sign-in-f @SIGN_IN-F]
     (u/assert sign-in-f "Missing sign-in-f")
     (sign-in-f opts))))

(defn sign-out> []
  (when-let [messaging-token @MESSAGING_TOKEN])
  (js/localStorage.removeItem "spark.uid")
  (u/=> (firebase-auth/signOut (auth))
        (fn [result]
          (u/later> 500
                    #(redirect-to-home))
          result)))

;; * Email

(defonce EMAIL_SIGN_IN (atom nil))

(defn send-sign-in-link-to-email [email url]
  (log ::sign-in-with-email
       :email email
       :url url)
  (let [settings {:url             url
                  :handleCodeInApp true}]
    (-> (firebase-auth/sendSignInLinkToEmail (auth) email (clj->js settings))
        (.then #(do
                  (js/localStorage.setItem "signInEmail" email)
                  (swap! EMAIL_SIGN_IN assoc
                         :email email
                         :status :waiting-for-email))
               #(reset! EMAIL_SIGN_IN {:error %})))))

(defn sign-in-with-email []
  (log ::sign-in-with-email
       :url (-> js/window.location.href))
  (reset! EMAIL_SIGN_IN {:status :input-email
                         :url    (-> js/window.location.href)}))

;; * Telephone
;; https://firebase.google.com/docs/auth/web/phone-auth?hl=en

(defonce TELEPHONE_SIGN_IN (atom nil))

(defn send-sign-in-code-to-telephone> [telephone]
  (-> (firebase-auth/signInWithPhoneNumber (auth) telephone js/window.recaptchaVerifier)
      (.then (fn [^js result]
               (log ::sign-in-code-sent-to-telephone
                    :result result)
               (swap! TELEPHONE_SIGN_IN assoc
                      :status :input-code
                      :confirmation-result result
                      :code (when goog.DEBUG "123456"))))
      (.catch (fn [^js error]
                ;; FIXME: reset recaptcha
                (swap! TELEPHONE_SIGN_IN assoc :error error)))))

(defn sign-in-with-telephone [telephone]
  (log ::sign-in-with-telephone
       :telephone telephone)
  (let [telephone (or telephone
                      (when goog.DEBUG "+16505551234"))]
    (reset! TELEPHONE_SIGN_IN {:status :input-telephone
                               :telephone telephone
                               :auto-send-sms (boolean (and telephone
                                                            (not goog.DEBUG)))
                               :url (-> js/window.location.href)})))

(defn sign-in-with-telephone-code [code]
  (let [confirmation-result (:confirmation-result @TELEPHONE_SIGN_IN)]
    (-> ^js confirmation-result
        (.confirm code)
        (.then (fn [^js result]
                 (log ::signed-in-with-telephone-code
                      :result result)
                 (js/window.location.reload)))
        (.catch (fn [^js error]
                  (swap! TELEPHONE_SIGN_IN assoc :error error))))))

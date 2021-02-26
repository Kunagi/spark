(ns spark.auth
  (:require
   [spark.logging :refer [log]]
   [spark.utils :as u]
   [spark.repository :as repository]))


(defonce SIGN_IN-F (atom nil))


(defonce AUTH_COMPLETED (atom false))
(defonce USER (atom nil))

(defn auth-completed? []
  @AUTH_COMPLETED)

(defn user []
  @USER)

(defn uid []
  (when-let [user (user)]
    (-> user .-uid)))


(def ^js firebase (-> js/window .-firebase))

(defn redirect [href]
  (js/window.location.replace href))

(defn redirect-to-home []
  (redirect "/"))


(defn process-sign-in-with-email-link []
  (let [auth (-> firebase .auth)
        href js/window.location.href]
    (when (-> auth (.isSignInWithEmailLink href))
      (log ::sign-in-with-email-link
           :href href)
      (let [email (or (js/window.localStorage.getItem "signInEmail")
                      (js/window.prompt "Bitte E-Mail zur Bestätigung eingeben"))]
        (-> auth
            (. signInWithEmailLink email href)
            (.then #(do
                      (js/window.localStorage.removeItem "signInEmail")
                      (redirect js/window.location.pathname))
                   #(do
                      (js/console.error %)
                      (redirect-to-home))))))))


(defn- email-domain [email]
  (when email (-> email (.substring (-> email (.indexOf "@") inc)))))


(defn update-user-doc [doc-schema ^js auth-user update-user]
  (log ::update-user-doc
       :doc-schema doc-schema )
  (let [uid (-> auth-user .-uid)
        email (-> auth-user .-email)]
    (repository/transact-doc-update>
     doc-schema uid
     (fn [db-user]
       (let [user (merge db-user
                         {:uid uid
                          :auth-email email
                          :auth-domain (email-domain email)
                          :auth-display-name (-> auth-user .-displayName)})
             user (u/update-if user update-user auth-user)]
         user)))))


(defn initialize [{:keys [user-doc-schema update-user set-user sign-in]}]
  (log ::initialize
       :doc-schema user-doc-schema)
  (reset! SIGN_IN-F sign-in)
  (let [auth (-> firebase .auth)]
    (-> auth (.useDeviceLanguage))
    (-> auth
        (.onAuthStateChanged
         (fn [^js user]
           (let [auth-completed? (auth-completed?)]
             (when-not auth-completed?
               (log ::auth-completed :user user))
             (when-not (= user @USER)
               (when auth-completed?
                 (log ::user-changed :user user)
                 (when-not user
                   (redirect-to-home)))
               (reset! USER user)
               (when user
                 (when user-doc-schema
                   (update-user-doc user-doc-schema user update-user)))
               (when set-user
                 (set-user user)))
             (when-not auth-completed?
               (reset! AUTH_COMPLETED true))))))
    (process-sign-in-with-email-link)))



(defn sign-in-with-google []
  (log ::sign-in-with-google)
  (let [GoogleAuthProvider (-> firebase .-auth .-GoogleAuthProvider)
        provider (GoogleAuthProvider.)]
    (.addScope ^js provider "openid")
    (.addScope ^js provider "profile")
    (.addScope ^js provider "email")
    (.addScope ^js provider "https://www.googleapis.com/auth/userinfo.email")
    (.addScope ^js provider "https://www.googleapis.com/auth/userinfo.profile")
    (-> firebase .auth (.signInWithRedirect ^js provider))))


(defn sign-in-with-microsoft []
  ;; https://firebase.google.com/docs/auth/web/microsoft-oauth?authuser=0
  ;; https://docs.microsoft.com/en-us/azure/active-directory/develop/quickstart-register-app
  ;; https://docs.microsoft.com/en-us/azure/active-directory/azuread-dev/v1-protocols-oauth-code
  ;; pplication (client) ID: fc6ee63d-375e-432c-b3d9-2722eba6a0ee
  ;; Directory (tenant) ID: 088fc83d-8363-42a6-8b7f-02c6f326cb17
  ;; Object ID; 5275d8a6-c7de-4367-a67f-7955ff60c4d6
  (let [AuthProvider (-> firebase .-auth .-OAuthProvider)
        provider (AuthProvider. "microsoft.com")]
    (-> ^js provider (.setCustomParameters #js {:prompt    "login"}))
    (-> firebase .auth (.signInWithRedirect ^js provider))))


(defn sign-in-with-facebook []
  ;; https://firebase.google.com/docs/auth/web/facebook-login
  ;; https://developers.facebook.com
  ;; https://developers.facebook.com/docs/permissions/reference
  (let [FacebookAuthProvider js/firebase.auth.FacebookAuthProvider
        provider (FacebookAuthProvider.)]
    (-> ^js provider (.addScope "email"))
    (-> firebase .auth
        (.signInWithRedirect ^js provider)
        (.then (fn [result]
                 (js/console.log "AUTH SUCCESS" result))
               (fn [result]
                 (js/console.log "AUTH FAILURE" result))))))


(defn sign-in []
  (@SIGN_IN-F))


(defn sign-out []
  (-> firebase .auth .signOut)
  (redirect-to-home))

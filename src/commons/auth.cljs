(ns commons.auth
  (:require
   [commons.logging :refer [log]]))


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


(defn initialize [{:keys [set-user sign-in]}]
  (log ::initialize)
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
               (set-user user))
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


(defn sign-in []
  (@SIGN_IN-F))


(defn sign-out []
  (-> firebase .auth .signOut)
  (redirect-to-home))

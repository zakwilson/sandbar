;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns examples.auth.form-auth
  "Simple example of using sandbar.auth with sandbar.form-authentication."
  (:use [ring.adapter.jetty :only (run-jetty)]
        [ring.middleware.file :only (wrap-file)]
        [ring.middleware.params :only (wrap-params)]
        [ring.middleware.file-info :only (wrap-file-info)]
        [compojure.core :only (defroutes GET ANY)]
        [sandbar.form-authentication :only (FormAuthAdapter form-authentication-routes form-authentication)]
        [sandbar.validation :only (add-validation-error)]
        [sandbar.auth :only (with-secure-channel with-security)]
        [sandbar.stateful-session :only (wrap-stateful-session)]
        [examples.auth.auth :only (layout home-view member-view admin-view permission-denied-view)]))

;; Building on the auth-demo code, adding form based
;; authentication. Please check out that demo first in order to
;; understand the basics of authentication. We are re-using the model
;; and view code from that example here. Notes are included.

;;
;; Properties
;; ==========
;; A map is used to configure the text that is displayed on the
;; login form as well as configuring the error messages that are
;; displayed. This is optional, there are sane defaults.

(def properties
     {:username "Username (admin or member)"
      :password "Password (same as above)"
      :username-validation-error "Enter either admin or member"
      :password-validation-error "Enter a password!"})

;;
;; Implement BasicAuthAdapter
;; ==========================
;;

(defrecord DemoAdapter []
  FormAuthAdapter
  (load-user
   [this username password]
   (let [login {:username username :password password}]
     (cond (= username "member")
           (merge login {:roles #{:member}})
           (= username "admin")
           (merge login {:roles #{:admin}})
           :else login)))
  (validate-password
   [this]
   (fn [m]
     (if (= (:password m) (:username m))
       m
       (add-validation-error m "Incorrect username or password!")))))

(defn form-authentication-adapter []
  (merge (DemoAdapter.) properties))

;;
;; Routes
;; ======
;; We add form-authentication-routes to get the login form, passing it our
;; layout, properties and the UserModel. We also create a route
;; for the permission-denied page which is where the user will be
;; redirected when they attempt to visit a page that they do not have
;; permission to view.

(defroutes my-routes
  (GET "/home*" [] (layout (home-view)))
  (GET "/member*" [] (layout (member-view)))
  (GET "/admin*" [] (layout (admin-view)))
  (GET "/permission-denied*" [] (layout (permission-denied-view)))
  (form-authentication-routes (fn [r c] (layout c))
                              (form-authentication-adapter))
  (ANY "*" [] (layout (home-view))))

;;
;; Configure Channel Security
;; ==========================
;; This will ensure that the login page is encrypted and all other
;; pages are not. We add a special config for resources so that we
;; don't get mixed content on secure pages.

(def security-config
     [#"/login.*" :ssl
      #".*.css|.*.js|.*.png|.*.gif" :any-channel
      #".*" :nossl])

;;
;; Add the with-security middleware to enable authorization and
;; authentication passing it the form-authentication function. We
;; also add middleware for channel security.

(def app
     (-> my-routes
         (with-security form-authentication)
         wrap-stateful-session
         wrap-params
         (wrap-file "public")
         wrap-file-info
         (with-secure-channel security-config 8080 8443)))

(defn run []
  (run-jetty (var app) {:join? false :ssl? true :port 8080 :ssl-port 8443
                        :keystore "my.keystore"
                        :key-password "foobar"})) 

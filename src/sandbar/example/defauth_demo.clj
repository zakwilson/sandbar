;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.defauth-demo
  "Simple example of using sandbar.auth with sandbar.form-authentication."
  (:use (ring.adapter jetty)
        (ring.middleware params file file-info)
        (compojure core)
        (hiccup core page-helpers)
        (sandbar core stateful-session auth validation)
        (sandbar.example [auth-demo :only (load-data-from
                                           layout
                                           home-view admin-view member-view
                                           permission-denied-view)]))
  (:require (sandbar form-authentication)))

(def properties
     {:username "Username (admin or member)"
      :password "Password (same as above)"
      :username-validation-error "Enter either admin or member"
      :password-validation-error "Enter a password!"})

(defauth form-auth
  :type :form
  :load (fn [username password]
          (let [login {:username username :password password}]
            (cond (= username "member")
                  (merge login {:roles #{:member}})
                  (= username "admin")
                  (merge login {:roles #{:admin}})
                  :else login)))
  :validator #(if (= (:password %) (:username %))
                %
                (add-validation-error % "Incorrect username or password!"))
  :properties properties)

(defroutes my-routes
  (GET "/home*" [] (layout (home-view)))
  (GET "/member*" [] (layout (member-view)))
  (GET "/admin*" [] (layout (admin-view)))
  (GET "/permission-denied*" [] (layout (permission-denied-view)))
  (form-auth (fn [r c] (layout c)))
  (ANY "*" [] (layout (home-view))))

(def security-config
     [#"/login.*"                   :ssl 
      #".*.css|.*.js|.*.png|.*.gif" :any-channel 
      #".*"                         :nossl])

(def app
     (-> my-routes
         (with-security form-auth)
         wrap-stateful-session
         (wrap-file "public")
         wrap-file-info
         (with-secure-channel security-config 8080 8443)))

(defn run []
  (run-jetty (var app) {:join? false :ssl? true :port 8080 :ssl-port 8443
                        :keystore "my.keystore"
                        :key-password "foobar"})) 

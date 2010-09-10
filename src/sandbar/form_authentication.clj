;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.form-authentication
  "Provide support for form based authentication."
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar [auth :only (logout!)]
                 [stateful-session :only (session-get
                                          session-put!
                                          set-flash-value!
                                          session-delete-key!)]
                 [core :only (property-lookup)]
                 [validation :only (build-validator
                                    non-empty-string
                                    if-valid)])
        (sandbar.dev [forms :only (form-layout-grid
                                   textfield
                                   password
                                   template
                                   get-params
                                   store-errors-and-redirect)])))

(defprotocol FormAuthAdapter
  "Protocol for processing form authenticaion credentials."
  (load-user [this username password] "Load user information based on the
    provided username and password. Return a map which contains at least a
    username and a set of roles under the keys :username and :roles.")
  (validate-password [this] "Create a validator function that will be
    used to validate the user's password. The user passed to this function
    will have been loaded from load-user."))

(defn form-authentication [request]
  (do (session-put! :auth-redirect-uri
                    (:uri request))
      (redirect "/login")))

(defn login-page [adapter request]
  (template :default
            (:uri request)
            {:buttons [[:submit "Login"]]}
            (form-layout-grid [1 1]
                              :login
                              [(textfield adapter :username {:size 25}
                                          :required)
                               (password adapter :password {:size 25}
                                         :required)]
                              request
                              {})))

(defn login-validator [adapter]
  (let [pw-validator (validate-password adapter)]
    (build-validator (non-empty-string :username :password adapter)
                     :ensure
                     pw-validator)))

(defn authenticate! [adapter params]
  (let [input (get-params [:username :password] params)
        user-data (load-user adapter (:username input) (:password input))
        success (or (session-get :auth-redirect-uri)
                    (property-lookup adapter :login-page))
        failure "login"]
    (redirect
     (if-valid (login-validator adapter) user-data
               #(do
                  (session-put! :current-user
                                {:name (:username %)
                                 :roles (:roles %)})
                  (session-delete-key! :auth-redirect-uri)
                  success)
               #(do (set-flash-value!
                     :login
                     (merge {:form-data (dissoc %1 :username :password)} %2))
                    failure)))))

(defn form-authentication-routes
  ([layout adapter]
     (form-authentication-routes "" layout adapter))
  ([path-prefix layout adapter]
     (routes
      (GET (str path-prefix "/login*") request
           (layout request
                   (login-page adapter request)))
      (POST (str path-prefix "/login*") {params :params}
            (authenticate! adapter params))
      (GET (str path-prefix "/logout*") []
           (logout! adapter)))))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-form-authentication
  (:use (clojure test)
        (ring.util [response :only (redirect)])
        (sandbar core
                 stateful-session
                 auth
                 test-fixtures
                 form-authentication
                 [test :only (t)])
        #_(sandbar.dev user-manager)))

(defn test-login-load-fn
  ([k] (test-login-load-fn k {} {}))
  ([k m1 m2]
     (cond (= k :app_user) [{:id 1 :username "u"
                             :password (hash-password "test" "cfjhuy")
                             :salt "cfjhuy"}] 
           (= k :role) [{:id 1 :name "admin"} {:id 2 :name "user"}]
           (= k :user_role) [{:user_id 1 :role_id 1}]
           :else {})))

#_(deftest test-login-validator
  (let [auth-source (form-authentication-adapter test-login-load-fn {})]
    (is (= ((login-validator auth-source)
            {:username "u"})
           {:_validation-errors {:password ["password cannot be blank!"]}
            :username "u"}))
    (is (= ((login-validator auth-source)
            {:password "test"})
           {:_validation-errors {:username ["username cannot be blank!"]}
            :password "test"}))
    (is (= ((login-validator auth-source)
            {:username "u" :password "testing" :salt "cfjhuy"})
           {:_validation-errors {:form ["Incorrect username or password!"]}
            :username "u" :password "testing" :salt "cfjhuy"}))
    (is (= ((login-validator auth-source)
            {:username "u" :password "test" :salt "cfjhuy"
             :password-hash (hash-password "test" "cfjhuy")})
           {:username "u" :password "test" :salt "cfjhuy"
            :password-hash (hash-password "test" "cfjhuy")}))
    (is (= ((login-validator auth-source)
            {:username "x" :password "test"})
           {:_validation-errors {:form ["Incorrect username or password!"]}
            :username "x" :password "test"}))))


#_(deftest test-authenticate!
  (let [auth-source (form-authentication-adapter test-login-load-fn {})]
    (t "authenticate!"
       (binding [sandbar-session (atom {})
                 sandbar-flash (atom {})]
         (t "with missing username"
            (is (= (authenticate! auth-source
                                  {"password" "x"})
                   (redirect "login"))))
        (t "with missing password"
           (is (= (authenticate! auth-source
                                 {"username" "u"})
                  (redirect "login")))))
      (t "with correct password"
         (binding [sandbar-flash (atom {})
                   sandbar-session (atom {:auth-redirect-uri "/test"})]
           (let [result (authenticate! auth-source
                                       {"username" "u" "password" "test"})]
             (is (= result
                    (redirect "/test")))
             (is (= @sandbar-session
                    {:current-user {:name "u"
                                    :roles #{:admin}}})))))
      (t "with incorrect password"
         (binding [sandbar-flash (atom {})
                   sandbar-session (atom {:auth-redirect-uri "/test"})]
           (let [result (authenticate! auth-source
                                       {"username" "u" "password" "wrong"})]
             (is (= result
                    (redirect "login")))
             (is (= (:auth-redirect-uri @sandbar-session)
                    "/test"))))))))

(deftest test-with-security-with-form-auth
  (binding [sandbar-session (atom {})
            sandbar-flash (atom {})]
    (t "with security using form authentication"
       (t "url config"
          (let [with-security (partial with-security
                                       :uri
                                       fixture-security-config)]
            (binding [app-context (atom "")]
            (t "redirect to login when user auth required and user is nil"
               (let [result ((with-security form-authentication)
                             {:uri "/admin/page"})]
                 (is (= result
                        (redirect "/login")))
                 (is (= (:auth-redirect-uri @sandbar-session)
                        "/admin/page"))))
            (t "redirect to login with a uri-prefix"
               (is (= ((with-security form-authentication "/prefix")
                       {:uri "/admin/page"})
                      (redirect "/prefix/login"))))
            (t "allow access when auth is not required"
               (is (= ((with-security form-authentication)
                       {:uri "/test.css"})
                      "/test.css")))
            (binding [sandbar-flash (atom {})
                      sandbar-session (atom {:current-user {:name "testuser"
                                                            :roles #{:user}}})]
              (t "redirect to permission denied when valid user without role"
                 (is (= ((with-security form-authentication)
                         {:uri "/admin/page"})
                        (redirect "/permission-denied"))))
              (t "allow access when user is in correct role"
                 (is (= ((with-security form-authentication)
                         {:uri "/some/page"})
                        "/some/page")))))))
      (t "and NO url config"
         (binding [app-context (atom "")]
           (t "redirect to permission denied when access exception is thrown"
              (is (= ((with-security
                        (fn [r] (access-error "testing with-security"))
                        []
                        form-authentication)
                      {:uri "/x"})
                     (redirect "/permission-denied"))))
           (t "redirect to login when authorization exception is thrown"
              (is (= ((with-security
                        (fn [r] (authentication-error "testing with-security"))
                        []
                        form-authentication)
                      {:uri "/x"})
                     (redirect "/login")))))))))

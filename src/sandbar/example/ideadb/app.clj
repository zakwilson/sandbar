;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.app
  (:use (compojure core)
        (ring.middleware [session :only (wrap-session)]
                         [file :only (wrap-file)])
        (ring.adapter jetty)
        (ring.middleware params stacktrace file file-info session)
        (sandbar core auth stateful-session
                 [form-authentication :only (form-authentication)])
        (sandbar.dev [standard-pages :only (page-not-found-404)]
                     [tables :only (wrap-table-js)])
        (sandbar.example.ideadb control
                                [layouts :only (main-layout)]))
    (:require (sandbar.example.ideadb [data :as data])))

(data/configure-database :development)

(def security-config
     [#"/admin.*"                   [:admin :ssl] 
      #"/idea/edit.*"               [:admin :ssl] 
      #"/idea/delete.*"             [:admin :ssl] 
      #"/idea/download.*"           :admin 
      #"/permission-denied.*"       :any
      #"/login.*"                   [:any :ssl] 
      #".*.css|.*.js|.*.png|.*.gif" [:any :any-channel] 
      #".*"                         [#{:admin :user} :nossl]])

(defroutes development-routes
  ideadb-routes
  (ANY "*" request (main-layout "404" request (page-not-found-404))))

(def app
     (-> development-routes
         (with-security security-config form-authentication)
         wrap-stateful-session
         wrap-params
         (wrap-file "public")
         wrap-file-info
         (wrap-table-js {:idea-table "/ideas"} :prototype)
         (with-secure-channel security-config 8080 8443)
         wrap-stacktrace))

(defn run []
  (run-jetty (var app) {:join? false :ssl? true :port 8080 :ssl-port 8443
                        :keystore "my.keystore"
                        :key-password "foobar"}))


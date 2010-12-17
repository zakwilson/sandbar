;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.forms2.example-basic
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [compojure.core :only [defroutes GET POST PUT]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!]]
        [sandbar.forms2]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   integer-number
                                   one-or-more-maps
                                   add-validation-error]]
        [sandbar.util :only [index-by]])
  (:require [compojure.route :as route]
            [examples.forms.database :as db]
            [examples.forms.views :as views]))

(def fields [(textfield :username :label "Username")
             (button :save)
             (button :cancel)])

(def user-form
     (let [form (form :user-form
                      :create-action "/users"
                      :update-action "/users/:id"
                      :update-method :put
                      :layout (grid-layout :title "Example Form"))]
       (embedded-form form fields)))

(defn user-form-page [request]
  (process-request user-form request))

(defroutes routes
  
  (GET "/users/new" request (views/layout (user-form-page request)))
  (POST "/users" request (user-form-page request))
  (GET "/users/:id/edit" request (views/layout (user-form-page request)))
  (PUT "/users/:id" request (user-form-page request))
  
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

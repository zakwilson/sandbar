;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns examples.forms.disintegrated
  "Demonstrates how to manually build a form using the default implementations
  of the form protocols (which are currently under development)."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [compojure.core :only [defroutes GET POST PUT]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!]]
        [sandbar.forms2]
        [sandbar.validation :only [build-validator
                                   non-empty-string]])
  (:require [compojure.route :as route]
            [examples.forms.database :as db]
            [examples.forms.views :as views]))

(def resource (restful-resource "/users" :id))

(def user-form (form :user-form
                     :resource resource
                     :layout (grid-layout)))

(def fields [(textfield :username :label "Username")
             (button :save)
             (button :cancel)])

(def user-form-page
     (form-page user-form
                resource
                fields
                (fn [_ body] (views/layout body))
                :load #(db/fetch %)))

(def responder
     (redirect-response user-form
                        "/"
                        #(do
                           (println "saving:" %)
                           (db/store %)
                           (flash-put! :user-message "User has been saved.")
                           "/")))

(def validator
     (build-validator (non-empty-string :username)))

(def user-form-submit
     (submit-handler fields
                     responder
                     :validator validator))

(defroutes routes

  (create-routes resource user-form-page user-form-submit)
  
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

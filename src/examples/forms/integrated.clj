;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns examples.forms.integrated
  "Demonstrates how to build a form using make-form."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware [params :only [wrap-params]]]
        [ring.middleware.file :only [wrap-file]]
        [compojure.core :only [defroutes GET]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!]]
        [sandbar.forms2]
        [sandbar.validation :only [build-validator
                                   non-empty-string]])
  (:require [compojure.route :as route]
            [examples.forms.database :as db]
            [examples.forms.views :as views]))

(defform user-form
  :resource (restful-resource "/users" :id)
  :fields [(textfield :username :label "Username")
           (button :save)
           (button :cancel)]
  :validator (build-validator (non-empty-string :username))
  :load #(db/fetch %)
  :page-layout (fn [_ body] (views/layout body))
  :on-cancel "/"
  :on-success #(do
                 (println "saving:" %)
                 (db/store %)
                 (flash-put! :user-message "User has been saved.")
                 "/"))

(defroutes routes
  
  user-form
  
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")
                            wrap-params))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

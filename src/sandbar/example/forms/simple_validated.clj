(ns sandbar.example.forms.simple-validated
  "Create a very simple form using sandbar.forms and validate form input.
   The purpose of this demo is to show how easy it is to create a form in
   the most simple case."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [compojure.core :only [defroutes GET]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         set-flash-value!]]
        [sandbar.forms :as forms]
        [sandbar.validation :only [add-validation-error]])
  (:require [compojure.route :as route]
            [sandbar.example.forms.database :as db]
            [sandbar.example.forms.views :as views]))

(defn validator [m]
  (if (< (count (:username m)) 5)
    (add-validation-error m :username "Username must have at least 5 chars.")
    m))

(forms/defform user-form "/user/edit"
  :fields [(forms/hidden :id)
           (forms/textfield "Username" :username)]
  :load #(db/find-user %)
  :on-cancel "/"
  :on-success
  #(do
     (db/store-user %)
     (set-flash-value! :user-message "User has been saved.")
     "/")
  :validator validator)

(defroutes routes
  (user-form (fn [_ form] (views/layout form)))
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

(ns sandbar.example.forms.static-extend
  "Extend a form that was created in another namespace. Add routes for the new
   form and use it on every request. This is an experimental feature. Some
   options are not yet supported."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [hiccup.page-helpers :only [link-to]]
        [compojure.core :only [defroutes GET]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         set-flash-value!]]
        [sandbar.core :only [get-param]]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   if-valid
                                   add-validation-error]])
  (:require [compojure.route :as route]
            [sandbar.forms :as forms]
            [sandbar.example.forms.database :as db]
            [sandbar.example.forms.views :as views]
            [sandbar.example.forms.complex :as complex]))

(defn home []
  (views/layout
   [:div
    (link-to "/altuser/edit" "Add New User")
    [:table
     [:tr
      [:th "Username"]
      [:th ""]]
     (map #(let [{:keys [username id]} %]
             [:tr
              [:td username]
              [:td (link-to (str "/altuser/edit/" id) "Edit")]])
          (db/all-things))]]))

(def admin-form-validator
     (build-validator
      (non-empty-string :admin-notes complex/properties)))

(forms/extend-form complex/user-form :with admin-form
    :at "/altuser/edit"
    :fields [(forms/textarea :admin-notes {:rows 5 :cols 70})]
    :validator admin-form-validator)

(defroutes routes
  (admin-form (fn [_ form] (views/layout form)))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

(ns sandbar.example.form-demo
  "Simple example of creating a form with Sandbar."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [defroutes GET POST]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         set-flash-value!]]
        [sandbar.core :only [icon stylesheet get-param]]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   if-valid
                                   add-validation-error]])
  (:require [compojure.route :as route]
            [sandbar.dev.forms :as forms]
            [sandbar.example.database :as db]))

(def properties
     {:username "Username"
      :password "Password"
      :first-name "First Name"
      :last-name "Last Name"
      :email "Email Address"
      :account-enabled "Account Enabled"
      :roles "Roles"
      :admin "Administrator"
      :user "User"
      :password-validation-error "Password must have at least 10 chars."})

(defn layout [content]
  (html
   (doctype :html4)
   [:html
    [:head
     (stylesheet "sandbar.css")
     (icon "icon.png")]
    [:body
     [:h2 "Sandbar Form Example"]
     content]]))

(defn home []
  (layout [:div
           (link-to "/user/edit" "Add New User")
           [:table
            [:tr
             [:th "Username"]
             [:th "Last Name"]
             [:th ""]]
            (map #(let [{:keys [username last-name id]} %]
                    [:tr
                     [:td username]
                     [:td last-name]
                     [:td (link-to (str "/user/edit/" id) "Edit")]])
                 (db/all-users))]]))

(defn password-strength [m]
  (if (< (count (:password m)) 10)
    (add-validation-error m :password properties)
    m))

(def validator
     (build-validator
      (non-empty-string :username :password :first-name :last-name :email
                        properties)
      :ensure
      password-strength))

;; -make sure the form always works so that you can build it up
;; gradually
;; -provide form field defaults
;; -provide default values
;; -specify the key of the id for the map
;; -provide your own layout
;; -provide your own buttons
;; -You should be able to internationalize the title and the buttons
;; -Add a text area
;; -Add a select
;; -Add a multi-select
;; -Create a custom control
;; -Hook it up to jQuery

(forms/defform user-form "/user/edit"
  :fields [(forms/hidden :id)
           (forms/textfield :username)
           (forms/password :password)
           (forms/textfield :first-name :last-name :email)
           (forms/checkbox :account-enabled)
           (forms/multi-checkbox :roles (db/all-roles) name)]
  :load #(db/find-user %)
  :on-cancel "/"
  :on-success
  #(do
     (db/store-user %)
     (set-flash-value! :user-message "User has been saved.")
     "/")
  :validator validator
  :properties properties
  :title #(case % :add "Create User" "Edit User")
  :buttons {:save "Save and Close" :save-and-new "Save and New"}
  :field-layout [1 1 2 1 1 1])

(defroutes routes
  (user-form (fn [_ form] (layout form)))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

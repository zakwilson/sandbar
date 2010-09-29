(ns sandbar.example.forms.runtime-extend
  "Create a complex form using sandbar.forms. Use extend-form to extend a form
   at runtime. Often you want to create a form that behaves differently
   depending on runtime conditions. In this example we extend a form so that
   when it is being edited, an additional field is displayed."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [defroutes GET POST]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!
                                         flash-get]]
        [sandbar.core :only [icon stylesheet get-param]]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   if-valid
                                   add-validation-error]])
  (:require [compojure.route :as route]
            [sandbar.forms :as forms]
            [sandbar.example.forms.database :as db]
            [sandbar.example.forms.views :as views]))

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
      :password-validation-error "Password must have at least 10 chars."
      :region "Region"
      :notes "Notes"
      :languages "Languages"
      :admin-notes "Administrator Notes"})

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

(forms/defform user-form "/user/edit"
  :fields [(forms/hidden :id)
           (forms/textfield :username)
           (forms/password :password)
           (forms/textfield :first-name :last-name :email)
           (forms/checkbox :account-enabled)
           (forms/multi-checkbox :roles (db/all-roles) name)
           (forms/select :region
                         (db/all-regions)
                         {:id :value :prompt {"" "Select a Region"}})
           (forms/multi-select :languages
                               (db/all-langs)
                               {:id :name}
                               {:class "something"})
           (forms/textarea :notes {:rows 5 :cols 70})]
  :buttons [[:save] [:cancel]]
  :load #(db/fetch %)
  :on-cancel "/"
  :on-success
  #(do
     (db/store %)
     (flash-put! :user-message "User has been saved.")
     "/")
  :validator validator
  :properties properties
  :style :over-under
  :title #(case % :add "Create User" "Edit User")
  :field-layout [1 1 2 1 1 1 2 1])

(def admin-form-validator
     (build-validator
      (non-empty-string :admin-notes properties)))

(forms/extend-form user-form :with admin-form
    :when (fn [action request form-data]
            (or (= action :edit)
                (get-param (-> request :params) :admin-notes)))
    :fields [(forms/textarea :admin-notes {:rows 5 :cols 70})]
    :validator admin-form-validator)

(defroutes routes
  (user-form (fn [_ form] (views/layout form)))
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

(ns sandbar.example.forms.dev.complex
  "Create a complex form using sandbar.forms. This demo shows most of
   the options that are available for defform."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [defroutes GET POST PUT]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!]]
        [sandbar.dev.forms]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   integer-number
                                   one-or-more-maps
                                   add-validation-error]])
  (:require [compojure.route :as route]
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

(def language-validator
     (build-validator
      (integer-number :id)
      (non-empty-string :name)))

#_(def validator
     (build-validator
      (non-empty-string :username :password :first-name :last-name :email
                        properties)
      (integer-number :region properties)
      (one-or-more-maps :languages language-validator properties)
      :ensure
      password-strength))

(def validator
     (build-validator
      (non-empty-string :username :password :first-name :last-name :email
                        properties)
      (integer-number :region properties)))

#_(defn marshal-user-form [params]
  (-> (forms/get-params [:id :username :password :first-name
                         :last-name :email :region :notes]
                        params)
      (forms/get-yes-no-fields params #{:account-enabled})
      (forms/get-multi-checkbox params :roles (db/all-roles) name)
      (forms/get-multi-select params :languages (db/all-langs) :id)
      forms/clean-form-input))

(defform user-form
  "Form for managing users."
  ;; don't know if passing the form-data is such a good idea when
  ;; fields is passed a fuction.
  :fields [(textfield :username)
           (password :password)
           (textfield :first-name)
           (textfield :last-name)
           (textfield :email)
           (checkbox :account-enabled)
           (multi-checkbox :roles
                           :source (db/all-roles)
                           :value name
                           :visible name)
           (select :region
                   :source (db/all-regions)
                   :prompt {"" "Select a Region"}
                   :value :id
                   :visible :value)
           #_(forms/multi-select properties
                                 :languages
                                 (db/all-langs)
                                 {:id :name})
           (textarea :notes :rows 5 :cols 80)]
  :buttons [[:save] [:cancel]]
  :load #(db/fetch %)
  ;; marshal will be passed a function that can wrap the generated
  ;; marshal function.
  #_:marshal #_marshal-user-form
  :on-cancel "/"
  :on-success
  #(do
     (println "saving:" %)
     (db/store %)
     (flash-put! :user-message "User has been saved.")
     "/")
  :validator validator
  :properties properties
  :style :over-under
  :title (fn [request] (if (get (-> request :params) "id")
                         "Edit User"
                         "Create User"))
  :layout [1 1 2 1 1 1]
  ;; defaults will need to specified in terms of the forms
  ;; representation not the external data
  #_:defaults #_{:email "unknown"
             :roles [:user]
             :account-enabled "Y"
             :languages [{:id 1 :name "Clojure"}]}
  :create-action "/users"
  :update-action "/users/:id"
  :update-method :put)

(defroutes routes
  
  (GET "/users/new" request (views/layout (user-form request)))
  (POST "/users" request (user-form request))
  (GET "/users/:id/edit" request (views/layout (user-form request)))
  (PUT "/users/:id" request (user-form request))
  
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

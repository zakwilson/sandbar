(ns sandbar.example.forms.without-macros
  "Create a simple form without using any of the macros in sandbar.forms.."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [defroutes GET POST]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!]]
        [sandbar.core :only [icon stylesheet get-param]]
        [sandbar.validation :only [build-validator non-empty-string
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

(def user-validator
     (build-validator
      (non-empty-string :username :password :first-name :last-name :email
                        properties)
      :ensure
      password-strength))

(def user-form-fields
     [(forms/hidden :id)
      (forms/textfield properties :username :required)
      (forms/password properties :password :required)
      (forms/textfield properties :first-name :required)
      (forms/textfield properties :last-name :required)
      (forms/textfield properties :email :required)
      (forms/checkbox properties :account-enabled)
      (forms/multi-checkbox properties :roles (db/all-roles) identity)
      (forms/select properties
                    :region
                    (db/all-regions)
                    {:id :value :prompt {"" "Select a Region"}})
      (forms/multi-select properties
                          :languages
                          (db/all-langs)
                          {:id :name})
      (forms/textarea properties :notes {:rows 5 :cols 80})])

(defn marshal-user-form [params]
  (-> (forms/get-params [:id :username :password :first-name
                         :last-name :email :region :notes]
                        params)
      (forms/get-yes-no-fields params #{:account-enabled})
      (forms/get-multi-checkbox params :roles (db/all-roles) name)
      (forms/get-multi-select params :languages (db/all-langs) :id)
      forms/clean-form-input))

(defn form-submission [{:keys [params uri] :as request}]
  (redirect
   (let [success "/"]
     (if (forms/form-cancelled? params)
       success
       (let [form-data (marshal-user-form params)
             failure (get (-> request :headers) "referer")]
         (if-valid user-validator form-data
                   #(do
                      (db/store %)
                      (flash-put! :user-message "User has been saved.")
                      success)
                   (forms/store-errors-and-redirect :user failure)))))))

(defn form-view [type {:keys [params] :as request}]
  (let [form-data (case type
                        :edit (let [id (get-param params :id)]
                                (db/fetch id))
                        {})]
    (views/layout
     (forms/template :over-under
                     "/user/edit"
                     {:title (case type :add "Create User" "Edit User")
                      :buttons [[:save] [:cancel]]}
                     (forms/form-layout-grid [1 1 2 1 1 1 2 1]
                                             :user
                                             user-form-fields
                                             request
                                             form-data)))))

(defroutes routes
  (GET "/user/edit/:id" request (form-view :edit request))
  (GET "/user/edit" request (form-view :add request))
  (POST "/user/edit" request (form-submission request))
  (GET "/" [] (views/home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

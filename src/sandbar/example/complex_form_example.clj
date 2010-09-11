(ns sandbar.example.complex-form-example
  "Simple example of creating a form using some of what Sandbar offers."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [defroutes GET POST]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         set-flash-value!
                                         get-flash-value]]
        [sandbar.core :only [icon stylesheet get-param]]
        [sandbar.forms :as forms]
        [sandbar.validation :only [build-validator non-empty-string
                                   if-valid
                                   add-validation-error]])
  (:require [compojure.route :as route]
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
      :password-validation-error "Password must have at least 10 chars."
      :region "Region"
      :notes "Notes"
      :languages "Languages"})

(defn layout [content]
  (html
   (doctype :html4)
   [:html
    [:head
     (stylesheet "sandbar.css")
     (stylesheet "sandbar-forms.css")
     (icon "icon.png")]
    [:body
     (if-let [m (get-flash-value :user-message)]
       [:div {:class "message"} m])
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

(def user-validator
     (build-validator
      (non-empty-string :username :password :first-name :last-name :email
                        properties)
      :ensure
      password-strength))

(def user-form-fields
     [(forms/textfield properties :username :required)
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

(defn marshal-user [params]
  (-> (forms/get-params [:id :username :password :first-name
                         :last-name :email :region :notes]
                        params)
      (forms/get-yes-no-fields params #{:account-enabled})
      (forms/get-multi-checkbox params :roles (db/all-roles) name)
      (forms/get-multi-select params :languages (db/all-langs) :id)
      forms/clean-form-input))

(defn form-submission [{:keys [params uri]}]
  (redirect
   (let [success "/"]
     (if (forms/form-cancelled? params)
       success
       (let [form-data (marshal-user params)
             failure uri]
         (if-valid user-validator form-data
                   #(do
                      (db/store-user %)
                      (set-flash-value! :user-message "User has been saved.")
                      success)
                   (forms/store-errors-and-redirect :user failure)))))))

(defn form-view [type {:keys [params] :as request}]
  (let [form-data (case type
                        :edit (let [id (get-param params :id)]
                                (db/find-user id))
                        {})]
    (layout
     (forms/template :over-under
                     "/user/edit"
                     {:title (case type :add "Create User" "Edit User")
                      :buttons [[:save] [:cancel]]}
                     (forms/form-layout-grid [1 1 2 1 1 1 2 1]
                                             :user
                                             (case type
                                                   :add user-form-fields
                                                   (conj user-form-fields
                                                         (forms/hidden :id)))
                                             request
                                             form-data)))))

(defroutes routes
  (GET "/user/edit/:id" request (form-view :edit request))
  (GET "/user/edit" request (form-view :add request))
  (POST "/user/edit" request (form-submission request))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

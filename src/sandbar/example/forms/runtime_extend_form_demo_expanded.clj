(ns sandbar.example.forms.complex-form-example
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
      :languages "Languages"
      :admin-notes "Administrator Notes"})

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

(def admin-form-validator
     (build-validator
      user-validator
      (non-empty-string :admin-notes properties)))

;; >>>>>>>>>>
;; The following code will be produced by defform

;; Define the default fields that will be displayed on the user form.

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

;; Multiplexing form types
;; Store functions that will determine form type in an atom. The
;; extend macro can produce code to add new functions to this atom.

(def user-form-types (atom []))

(defn user-form-type
  "Returns the form type based on the current request and form-data. Returns
   nil if no type is found."
  [action request form-data]
  (first (filter #(not (= % :default))
                 (map #(% action request form-data) @user-form-types))))

;; Multimethod definitions that are used for different form types

(defn user-form-dispatch [& args] (first args))
(defmulti get-user-form-fields user-form-dispatch)
(defmulti marshal-user-form user-form-dispatch)
(defmulti get-user-form-layout user-form-dispatch)
(defmulti get-user-form-validator user-form-dispatch)
(defmulti get-user-form-title user-form-dispatch)
(defmulti get-user-form-buttons user-form-dispatch)

;; Default implementations of said multimethods

(defmethod get-user-form-fields :default [form-type]
           user-form-fields)

(defmethod marshal-user-form :default [form-type params]
           (-> (forms/get-params [:id :username :password :first-name
                                  :last-name :email :region :notes]
                                 params)
               (forms/get-yes-no-fields params #{:account-enabled})
               (forms/get-multi-checkbox params :roles (db/all-roles) name)
               (forms/get-multi-select params :languages (db/all-langs) :id)
               forms/clean-form-input))

(defmethod get-user-form-layout :default [form-type]
           [1 1 2 1 1 1 2 1])

(defmethod get-user-form-validator :default [form-type]
           user-validator)

(defmethod get-user-form-title :default [form-type type]
           (case type :add "Create User" "Edit User"))

(defmethod get-user-form-buttons :default [form-type]
           [[:save] [:cancel]])

;; Functions to submit and view a form. Anything that may be
;; extended will use one of the multimethods above and call
;; user-form-type to get the type for the current request.

(defn form-submission [{:keys [params uri] :as request}]
  (redirect
   (let [success "/"
         form-type (user-form-type :marshal request {})]
     (if (forms/form-cancelled? params)
       success
       (let [form-data (marshal-user-form form-type params)
             failure (get (-> request :headers) "referer")
             form-type (user-form-type :validate request form-data)]
         (if-valid (get-user-form-validator form-type) form-data
                   #(do
                      (db/store-user %)
                      (set-flash-value! :user-message "User has been saved.")
                      success)
                   (forms/store-errors-and-redirect :user failure)))))))

(defn form-view [type {:keys [params] :as request}]
  (let [form-data (case type
                        :edit (let [id (get-param params :id)]
                                (db/find-user id))
                        {})
        form-type (user-form-type type request form-data)]
    (layout
     (forms/template :over-under
                     "/user/edit"
                     {:title (get-user-form-title form-type type) 
                      :buttons (get-user-form-buttons form-type)}
                     (forms/form-layout-grid (get-user-form-layout form-type)
                                             :user
                                             (get-user-form-fields form-type)
                                             request
                                             form-data)))))

;; End of code generated by defform
;; <<<<<<<<<<

;; >>>>>>>>>>
;; Beginning of code generated by a call to extend, when you are
;; extending user-form with admin-form. Changes the behavior of the
;; form at runtime without modifing the code above. Add the ability to
;; extend the layout, validation function, buttons and title.

(swap! user-form-types (fn [a b] (conj a b))
       (fn [action request form-data]
         (if (or (= action :edit)
                 (get-param (-> request :params) :admin-notes))
           :admin-form
           :user-form)))

(def admin-form-fields
     [(forms/textarea properties :admin-notes {:rows 5 :cols 80} :required)])

(defmethod get-user-form-fields :admin-form [form-type]
           (concat (get-user-form-fields :user-form)
                   admin-form-fields))

(defmethod marshal-user-form :admin-form [form-type params]
           (merge (marshal-user-form :user-form params)
                  (get-params [:admin-notes] params)))

(defmethod get-user-form-validator :admin-form [form-type]
           admin-form-validator)

;; End of code generated by extend.
;; <<<<<<<<<<

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

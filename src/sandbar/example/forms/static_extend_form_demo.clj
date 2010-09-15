(ns sandbar.example.forms.runtime-extend-form-demo
  "Create a complex form using sandbar.forms. Demonstrates how to use
   extend-form to extend a form at runtime."
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
        [sandbar.validation :only [build-validator
                                   non-empty-string
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
  :load #(db/find-user %)
  :on-cancel "/"
  :on-success
  #(do
     (db/store-user %)
     (set-flash-value! :user-message "User has been saved.")
     "/")
  :validator validator
  :properties properties
  :style :over-under
  :title #(case % :add "Create User" "Edit User")
  :field-layout [1 1 2 1 1 1 2 1]
  :defaults {:username "something"
             :roles [:user]
             :account-enabled "Y"
             :region 2
             :languages [{:id 1 :name "Clojure"}]})

;; The commented code below works. Create some new examples that show
;; how to use these in a real scenario.

(def admin-form-validator
     (build-validator
      (non-empty-string :admin-notes properties)))

#_(forms/extend-form user-form :with admin-form
    :when (fn [action request form-data]
            (or (= action :edit)
                (get-param (-> request :params) :admin-notes)))
    :fields [(forms/textarea :admin-notes {:rows 5 :cols 70})]
    :validator admin-form-validator)

#_(forms/extend-form user-form :with admin-form
    :at "/user-two/edit"
    :fields [(forms/textarea :admin-notes {:rows 5 :cols 70})]
    :validator admin-form-validator)

(defroutes routes
  (user-form (fn [_ form] (layout form)))
  #_(admin-form (fn [_ form] (layout form)))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

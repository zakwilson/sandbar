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
                                   add-validation-error]]
        [sandbar.util :only [index-by]])
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
      #_(integer-number :region properties)))

;; For multi feilds, the source will always have all values. Add a
;; filter option for fields in case you do not want to show all values
;; based on the current request.

(defform user-form
  "Form for managing users."
  :fields [(textfield :username)
           (password :password)
           (textfield :first-name)
           (textfield :last-name)
           (textfield :email)
           (checkbox :account-enabled)
           ;; Document - use bindings for setting the data sources and
           ;; mappings for the next three fields.
           (multi-checkbox :roles)
           (select :region :prompt {"" "Select a Region"})
           (multi-select :languages)
           (textarea :notes :rows 5 :cols 80)]
  ;; Document - value and visible default to name and identity. This
  ;; makes it easy to use lists of keywords as data sources. The data
  ;; source is a function of the request. Be careful because the data
  ;; source will be generated for both the get and post/put
  ;; requests. the :data function will determine how data is
  ;; represented in the map and defaults to identity. bindings has
  ;; been pulled out because it is used for both displaying the form
  ;; and marshaling data.
  :bindings {:roles     {:source (constantly (db/all-roles))}
             :region    {:value :id
                         :visible :value
                         :source (constantly (db/all-regions))
                         :data :id}
             :languages {:value :id
                         :visible :name
                         :source (constantly (db/all-langs))}}
  :buttons [[:save] [:cancel]]
  :load #(db/fetch %)
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
  :layout [1 1 2 1 1 1 2]
  :defaults {:email "unknown"
             :roles [:user]
             :account-enabled true
             :region 3
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

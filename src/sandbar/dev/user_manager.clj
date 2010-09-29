;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.user-manager
  (:use (ring.util [response :only (redirect)])
        (hiccup core)
        (compojure core)
        (sandbar core
                 util
                 stateful-session
                 validation
                 forms
                 form-authentication
                 [auth :only (hash-password)])
        (sandbar.dev tables 
                     standard-pages)))

(defn secure-user
  "Ensure that the user has a salt value associated with it and that if the
   password has changed, it is hashed."
  [new-user old-user]
  (let [password (:password new-user)
        salt (if-let [s (:salt old-user)]
               s
               (random-string 12 12))]
    (if (not (= (:password old-user) password))
      (-> new-user
         (assoc :salt salt)
         (assoc :password (hash-password password salt)))
      new-user)))

(def user-table-columns
     [{:column :last_name :actions #{:sort}}
      {:column :first_name :actions #{:sort}}
      {:column :username :actions #{:sort}}
      {:column :email :actions #{:sort}}
      :empty])

(defmethod display-table-cell [:app_user :empty] [type k data]
  [:div
   (clink-to (str "edit?id=" (:id data)) "Edit") ", "
   (clink-to (str "delete?id=" (:id data)) "Delete")])

(defrecord UserTable [type props load-fn]
  ResourceList
  
  (find-resources
   [this filters page-and-sort]
   (load-fn type filters page-and-sort))
  
  (fields [this] [])

  Labels

  (label [this key] (get props key (name key))))

(defn user-table [props load-fn request]
  (filter-and-sort-table (UserTable. :app_user
                                     props
                                     load-fn)
                         user-table-columns
                         (:params request)))

(defn user-list-page [props load-fn request]
  [:div
   [:div (clink-to "new" "Add new User")]
   (user-table props load-fn request)])

(defn user-form-fields [load-fn props]
  [(textfield props :username :required)
   (password props :new_password :required)
   (textfield props :first_name :required)
   (textfield props :last_name :required)
   (textfield props :email :required)
   (checkbox props :account_enabled)
   (multi-checkbox props :roles (load-fn :role) :name)])

(defn edit-user-form [data-fns props request]
  (let [lookup-fn (fn [r] ((data-fns :lookup) :app_user
                           (get-param (:params r) :id)))
        action (if (.endsWith (:uri request) "new") :new :edit)
        form-data (if (= action :new) {} (lookup-fn request))
        title (if (= action :new) "Create New User" "Edit User")]
    (template :over-under
              (name action)
              {:title title
               :buttons [[:save] [:cancel]]}
              (form-layout-grid [1 1 2 1 1 1]
                                :user
                                (conj
                                 (user-form-fields (data-fns :load) props)
                                 (hidden :id)
                                 (hidden :password))
                                request
                                (if (= action :edit)
                                  (assoc form-data :new_password "_unchanged")
                                  form-data)))))

(defn create-user-from-params [load-fn params]
  (let [user (-> (get-params [:id :username :new_password :password
                              :first_name :last_name :email]
                             params)
                 (get-yes-no-fields params #{:account_enabled})
                 (get-multi-checkbox params :roles (load-fn :role) :name)
                 clean-form-input)
        user (if (= "_unchanged" (:new_password user))
               user
               (assoc user :password (:new_password user)))]
    (println user)
    user))

(defn user-validator [props]
  (build-validator (non-empty-string :username
                                     :new_password
                                     :first_name
                                     :last_name
                                     :email props)))

(defn save-user! [data-fns props request]
  (redirect
   (let [params (:params request)
         success "list"]
     (if (form-cancelled? params)
       success
       (let [save-or-update-fn (data-fns :save)
             form-data (create-user-from-params (data-fns :load) params)
             failure (cpath (:uri request))]
         (if-valid (user-validator props) form-data
                   #(do
                      (save-or-update-fn :app_user (dissoc % :new_password))
                      (flash-put! :user-message
                                        "User has been saved.")
                      success)
                   (store-errors-and-redirect :user failure)))))))

;;
;; Functions for working with users
;; ================================
;;

(defrecord DefaultFormAuthAdapter [load-fn]
  FormAuthAdapter
  (load-user
   [this username password]
   (let [user (first (load-fn :app_user
                              {:username username} {}))
         roles (index-by :id (load-fn :role))]
     (-> {:username username :password password}
         (assoc :password-hash (:password user))
         (assoc :salt (:salt user))
         (assoc :roles (set
                        (map #(keyword (:name (roles %)))
                             (map :role_id
                                  (load-fn :user_role
                                           {:user_id (:id user)} {}))))))))
  (validate-password
   [this]
   (fn [m]
     (if (and (:salt m)
              (:password-hash m)
              (= (hash-password (:password m) (:salt m))
                 (:password-hash m)))
       m
       (add-validation-error m "Incorrect username or password!")))))

(defn form-authentication-adapter [load-fn props]
  (merge (DefaultFormAuthAdapter. load-fn) props))

;;
;; Routes
;; ======
;;

(defn security-edit-user-routes [path-prefix layout name-fn props data-fns]
  (routes
   (GET (str path-prefix "/user/list*") request
        (layout (name-fn request)
                request
                (user-list-page props (data-fns :load) request)))
   (POST (str path-prefix "/user/list*") request
         (table-as-json (html (user-table props (data-fns :load) request))))
   (GET (str path-prefix "/user/new*") request
        (layout (name-fn request)
                request
                (edit-user-form data-fns props request)))
   (POST (str path-prefix "/user/new*") request
         (save-user! data-fns props request))
   (GET (str path-prefix "/user/edit*") request
        (layout (name-fn request)
                request
                (edit-user-form data-fns props request)))
   (POST (str path-prefix "/user/edit*") request
         (save-user! data-fns props request))
   (GET (str path-prefix "/user/delete*") request
        (layout (name-fn request)
                request
                (confirm-delete (data-fns :lookup)
                                :app_user
                                (fn [u]
                                  (str (:first_name u) " " (:last_name u)))
                                props
                                (get-param (:params request) :id))))
   (POST (str path-prefix "/user/delete*") {params :params}
         (do
           (if (not (form-cancelled? params))
             ((data-fns :delete) :app_user (get-param params :id)))
           (redirect "list")))))

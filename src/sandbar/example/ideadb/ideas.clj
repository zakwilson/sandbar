;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.ideas
  (:require [sandbar.example.ideadb.data :as data])
  (:use (hiccup core)
        (ring.util [response :only (redirect)])
        (sandbar core
                 [auth :only (current-username
                              current-user-roles
                              any-role-granted?)]
                 stateful-session
                 util
                 validation)
        (sandbar.dev tables forms standard-pages)
        (sandbar.example.ideadb properties layouts)))

;;
;; Idea List
;; =========
;;

(def idea-table-columns
     [{:column :id :actions #{:sort}}
      :name
      :description
      :customer_need
      {:column :originator :actions #{:sort :filter}} 
      {:column :date_entered :actions #{:sort :filter} :attr {:align :center}} 
      {:column :category :actions #{:sort :filter}} 
      {:column :idea_type :actions #{:sort :filter}} 
      {:column :business_unit :actions #{:sort :filter}} 
      {:column :status :actions #{:sort :filter}}])

(defn generate-welcome-message [request]
  (if (not (data/admin-role? request))
    [:div {:id "welcome-message"}
     (str "Welcome " (current-username)
          "! The table below displays all of the ideas you have submitted.")]))

(defn idea-table [request]
  (let [admin (data/admin-role? request)]
    (filter-and-sort-table
     (:params request)
     {:type :idea :name :idea-table :props properties}
     (if admin 
       (conj idea-table-columns :empty)
       idea-table-columns)
     (fn [k row-data]
       (cond (= k :name)
             (if admin
               (clink-to (str "/idea/edit?id=" (:id row-data))
                         (:name row-data))
               (:name row-data))
             (= k :empty)
             (clink-to (str "/idea/delete?id=" (:id row-data)) "Delete")
             :else (or (k row-data) "")))
     (data/idea-table-records-function request))))

(defn idea-list-view [request]
  (generate-welcome-message request)
  (html
   (idea-table request)))

(defn user-has-ideas? [request]
  (< 0 (count ((data/idea-table-records-function request) :idea {} {}))))

(defn idea-list [request]
  (if (or (data/admin-role? request)
          (user-has-ideas? request))
    (list-layout "Idea List"
                 request
                 (idea-list-view request))
    (redirect (cpath "/idea/new"))))

(defn idea-list-post [request]
  (table-as-json (html (idea-table request))))

(defn idea-download-view []
  (let [data (data/fetch :idea)
        fields [:id :name :description :customer_need :originator
                :date_entered :category :idea_type :business_unit :status]
        data (concat [(map #(% properties) fields)]
                     (map #(map (fn [field] (% field)) fields) data))]
    (format-csv data)))

;; This should be included in the above function. It is the same view
;; but a differnet content type.
(defn idea-download [request]
  {:status 200
   :headers {"Content-Type" "application/vnd.ms-excel"
             "Content-disposition"
             "attachment;filename=\"ideadb.csv\""}
   :body (idea-download-view)})

;;
;; Create Idea
;; ===========
;;

(defn public-idea-fields []
  [(form-textarea "What is your idea?  Describe your new idea in 100 words
                   or less?"
                  :description {:cols 75 :rows 10} :required)
   (form-textfield "Do you have a name for your idea?"
                   :name {:size 70} :required)
   (form-textarea "What consumer need would be met by this new idea?"
                  :customer_need {:cols 75 :rows 5} :required)
   (form-textfield "Who can we thank for this idea? (Optional)"
                   :originator {:size 70})])

(defn admin-idea-fields []
  [(form-select "Category"
               :category :name :name
               (data/fetch :idea_category :order-by :name)
               {}
               {"" "Select a Category..."})
   (form-select "Type"
                :idea_type :name :name
                (data/fetch :idea_type :order-by :name)
                {}
                {"" "Select a Type..."})
   (form-select "Business Unit"
                :business_unit :name :name
                (data/fetch :business_unit :order-by :name)
                {}
                {"" "Select a Business Unit..."})
   (form-select "Status"
                :status :name :name
                (data/fetch :idea_status :order-by :name)
                {}
                {"" "Select a Status..."})])

(defn new-idea-form [request]
  (let [admin (data/admin-role? request)]
    (standard-form "Submit an Idea" "/idea/new" 
                   (if admin
                     {:submit "Save and Close" :submit-and-new "Save and New"}
                     "Submit My Idea") 
                   (form-layout-grid [1 1 1 1 4]
                                     :idea
                                     (if admin
                                       (concat
                                        (public-idea-fields)
                                        (admin-idea-fields))
                                       (public-idea-fields))
                                     request))))

(defn new-idea [request]
  (form-layout "New Idea Form"
               request
               (new-idea-form request)))

(defn create-idea-from-params [params]
  (let [idea
        (get-params [:id :description :name :customer_need :originator
                     :category :idea_type :business_unit :status
                     :date_entered :user_id]
                    params)
        date (if-let [de (:date_entered idea)]
               (if (empty? de) (date-string) de)
               (date-string))
        user (if-let [u (:user_id idea)]
               (if (empty? u) (current-username) u)
               (current-username))
        idea (-> idea
                 (assoc :date_entered date)
                 (assoc :user_id user))]
    (clean-form-input idea)))

(defn save-idea-success-fn [action success]
  (fn [form-data]
    (do
      (println form-data)
      (data/save :idea form-data)
      (set-flash-value! :user-message (if (= action "new")
                                        "Your idea has been successfully
                                         submitted."
                                        "The idea has been updated."))
      success)))

(def idea-validator
     (build-validator (non-empty-string :description
                                        "Please enter a description.")
                      (non-empty-string :name
                                        "Please enter a name.")
                      (non-empty-string :customer_need
                                        "Please enter a customer need.")))
(defn save-idea! [params action]
  (redirect
   (let [submit (get params "submit")
         success (if (= submit "Save and New")
                   (cpath "/idea/new")
                   (cpath "/ideas"))]
     (if (form-cancelled? params)
       success
       (let [form-data (create-idea-from-params params)
             failure (cpath (str "/idea/" action))]
         (if-valid idea-validator form-data
                   (save-idea-success-fn action success)
                   (store-errors-and-redirect :idea failure)))))))

(defn new-idea-post [{params :params}]
  (save-idea! params "new"))

;;
;; Edit Idea
;; =========
;;

(defn edit-idea-form [request params]
  (let [form-data (data/fetch-id :idea (get params "id"))]
    (standard-form "Administrator Form" "/idea/edit"
                   "Save Changes"
                   (form-layout-grid [1 1 1 1 4]
                                     :idea
                                     (conj
                                      (concat (public-idea-fields)
                                              (admin-idea-fields))
                                      (form-hidden :id)
                                      (form-hidden :date_entered)
                                      (form-hidden :user_id))
                                     request
                                     form-data))))

(defn edit-idea [request]
  (form-layout "Edit Idea Form"
               request
               (edit-idea-form request
                               (:params request))))

(defn edit-idea-post [{params :params}]
  (save-idea! params (str "edit?id=" (get params "id"))))

;;
;; Delete Idea
;; ===========
;;

(defn delete-idea [request]
  (form-layout "Confirm Delete Idea"
               request
               (confirm-delete data/fetch-id
                               :idea
                               properties
                               (get (:params request) "id"))))

(defn delete-idea-post [{params :params}]
  (do
    (if (not (form-cancelled? params))
      (data/delete-id :idea (get params "id")))
    (redirect "list")))

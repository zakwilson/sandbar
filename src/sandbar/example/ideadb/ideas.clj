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
                 validation
                 forms)
        (sandbar.dev tables standard-pages)
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
  (if (not (any-role-granted? :admin))
    [:div {:id "welcome-message"}
     (str "Welcome " (current-username)
          "! The table below displays all of the ideas you have submitted.")]))

(defmethod display-table-cell [:idea :name] [type k data]
           (if (any-role-granted? :admin)
             (clink-to (str "/idea/edit/" (:id data))
                       (:name data))
             (:name data)))

(defmethod display-table-cell [:idea :empty] [type k data]
           (clink-to (str "/idea/delete?id=" (:id data)) "Delete"))

;; TODO - Add a feature to Carte that will allow you to ensure that a
;; criteria is met no matter what previous criteria have been set.
;; Carte should also be able to deal with an empty criteria list.

(defrecord IdeaTable [type props page-size]

  ResourceList
  
  (find-resources
   [this filters page-and-sort]
   (data/idea-table-records-function type filters page-and-sort))

  (fields [this] [])

  PagedResources

  (page-size [this] page-size)
  
  (total-resource-count [this filters]
                        (data/count-records type filters))

  Labels

  (label [this key] (get props key (name key))))

(def idea-table-adapter (IdeaTable. :idea properties 10))

(defn idea-table [request]
  (filter-and-sort-table idea-table-adapter
                         (if (any-role-granted? :admin)
                           (conj idea-table-columns :empty)
                           idea-table-columns)
                         (:params request)))

(defn idea-list-view [request]
  (html
   (generate-welcome-message request)
   (idea-table request)))

(defn user-has-ideas? [request]
  (< 0 (count (data/idea-table-records-function :idea {} {}))))

(defn idea-list [request]
  (if (or (any-role-granted? :admin)
          (user-has-ideas? request))
    (list-layout "Idea List"
                 request
                 (idea-list-view request))
    (redirect (cpath "/idea/edit"))))

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
;; Create Form
;; ===========
;;

(def idea-properties
     {:description "What is your idea?  Describe your new idea in 100 words
                    or less?"
      :description-validation-error "Please enter a description."
      :name "Do you have a name for your idea?"
      :name-validation-error "Please enter a name."
      :customer_need "What consumer need would be met by this new idea?"
      :customer_need-validation-error "Please enter a customer need."
      :originator "Who can we thank for this idea? (Optional)"
      :category "Category"
      :idea_type "Type"
      :business_unit "Business Unit"
      :status "Status"})

(def idea-validator
     (build-validator (non-empty-string :description :name :customer_need
                                        idea-properties)))

(defform idea-form "/idea/edit"
  :fields [(hidden :id)
           (hidden :date_entered)
           (hidden :user_id)
           (textarea :description {:cols 75 :rows 10})
           (textfield :name {:size 70})
           (textarea :customer_need {:cols 75 :rows 5})
           (textfield :originator {:size 70})]
  :style :over-under
  :title #(case % :add "Submit an Idea" "Edit an Idea")
  :buttons [[:submit "Submit My Idea"] [:cancel]]
  :field-layout [1 1 1 1 4]
  :on-cancel "/ideas"
  :on-success
  #(do
     (data/save :idea %)
     (set-flash-value! :user-message "Your idea has been saved.")
     "/ideas")
  :load #(data/fetch-id :idea %)
  :properties idea-properties
  :validator idea-validator
  :defaults {:date_entered (date-string)
             :user_id (current-username)})

(extend-form idea-form :with admin-idea-form
  :when (fn [& args] (any-role-granted? :admin))
  :fields [(select :category 
                   (data/fetch :idea_category :order-by :name)
                   {:name :name :prompt {"" "Select a Category..."}})
           (select :idea_type
                   (data/fetch :idea_type :order-by :name)
                   {:name :name :prompt {"" "Select a Type..."}})
           (select :business_unit 
                   (data/fetch :business_unit :order-by :name)
                   {:name :name :prompt {"" "Select a Business Unit..."}})
           (select :status 
                   (data/fetch :idea_status :order-by :name)
                   {:name :name :prompt {"" "Select a Status..."}})]
  :title (fn [_] "Administrator Form")
  :buttons [[:submit "Save and Close" ]
            [:submit-and-new "Save and New"]
            [:cancel]])

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
                               (get-param (:params request) :id))))

(defn delete-idea-post [{params :params}]
  (do
    (if (not (form-cancelled? params))
      (data/delete-id :idea (get-param params :id)))
    (redirect "list")))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.data
  (:use [clojure.contrib.str-utils :only (re-split re-partition)]
        (sandbar [auth :only (current-user
                              ensure-any-role-if
                              any-role-granted?)])
        (carte model))
  (:require [carte.core :as carte]))

;; TODO - I want to require model here but the model macro will not
;; allow me to do that. Fix this in Carte.

(def db (atom nil))

;; TODO - Update the database to use more associations and be a better
;; example of carte usage.

(def idea-model
     (model
      (app_user [:id :username :password :salt :first_name :last_name :email
                 :account_enabled]
                (many-to-many roles :role :=> :user_role :user_id :role_id))
      (role [:id :name])
      (idea [:id :name :description :customer_need :originator :date_entered
             :archive :business_unit :category :status :idea_type])
      (business_unit [:id :name])
      (idea_category [:id :name])
      (idea_status [:id :name])
      (idea_type [:id :name])))

(defn get-connection-info [context]
  {:connection
   {:classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//localhost/idea_db"
    :user "idea_user"
    :password "123456789"}})

(defn configure-database [context]
  (if (not @db)
    (swap! db (fn [a b] b) (merge idea-model
                                  (get-connection-info context)))))

(defn fetch [& body]
  (println "fetch:" body)
  (apply carte/fetch @db body))

(defn fetch-one [& body]
  (apply carte/fetch-one @db body))

(defn fetch-id [table id]
  (carte/fetch-one @db table {:id id}))

(defn save [& body]
  (println "save:" body)
  (apply carte/save-or-update @db body))

(defn delete [& body]
  (apply carte/delete-record @db body))

(defn delete-id [type id]
  (if-let [record (fetch-id type id)]
    (carte/delete-record @db type record)))

(defn admin-role? [request]
  (any-role-granted? :admin))


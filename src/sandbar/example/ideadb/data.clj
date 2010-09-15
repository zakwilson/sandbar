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
                              current-username
                              ensure-any-role-if
                              any-role-granted?)]
                 [core :only (property-lookup)])
        (sandbar.dev [tables :as tables]))
  (:require (carte [core :as carte]
                   [model :as model])))

(def db (atom nil))

;; TODO - Update the database to use more associations and be a better
;; example of Carte usage.

(def idea-model
     (model/model
      (app_user (many-to-many roles :role :=> :user_role :user_id :role_id))
      (idea)
      (business_unit)
      (idea_category)
      (idea_status)
      (idea_type)))

(defn get-connection-info [context]
  {:connection
   {:classname "com.mysql.jdbc.Driver"
    :subprotocol "mysql"
    :subname "//localhost/idea_db"
    :user "idea_user"
    :password "123456789"}})

(defn configure-database [context]
  (if (not @db)
    (swap! db (fn [a b] b) (carte/merge-with-attrs
                             (get-connection-info context)
                             idea-model))))

(defn fetch [& body]
  (println "fetch:" body)
  (apply carte/fetch @db body))

(defn fetch-one [& body]
  (println "fetch-one:" body)
  (apply carte/fetch-one @db body))

(defn fetch-id [table id]
  (carte/fetch-one @db table {:id id}))

(defn save
  ([m]
     (if (contains? m :type)
       (save (:type m) (dissoc m :type))
       (save m)))
  ([arg & body]
     (apply carte/save-or-update @db arg body)))

(defn delete [& body]
  (apply carte/delete-record @db body))

(defn delete-id [type id]
  (if-let [record (fetch-id type id)]
    (carte/delete-record @db type record)))

(defn fetch [& body]
  (println "fetch:" body)
  (apply carte/fetch @db body))

(defn count-records [table filters]
  (apply carte/count-records @db
         (tables/carte-table-adapter table
                                     filters
                                     {})))

(defn idea-table-records-function [type filters sort-and-page]
  (let [filters (if (not (any-role-granted? :admin))
                  (merge filters
                         {:user_id (current-username)})
                  filters)]
    (apply fetch (tables/carte-table-adapter type
                                             filters
                                             sort-and-page))))

(defn simple-list [type properties]
  {:paged-list (fn [filters] (if (empty? filters)
                               (fetch type)
                               (fetch type filters)))
   :find-by-id (fn [id] (fetch-id type id))
   :save (fn [m] (save m))
   :delete-by-id (fn [id] (delete-id type id))
   :visible-name (property-lookup properties type)
   :id type
   :properties properties})


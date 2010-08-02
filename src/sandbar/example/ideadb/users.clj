;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.users
  (:require [sandbar.example.ideadb.data :as data]
            [sandbar.dev [tables :as tables]])
  (:use (sandbar core util)
        (sandbar.dev forms user-manager)
        (sandbar.example.ideadb properties)))

(defn user-data-functions [k]
  (cond (= k :save)
        (fn [type m]
          (if (= :app_user type)
            (data/save type (secure-user m (data/fetch-id type (:id m))))
            (data/save type m)))
        
        (= k :load)
        (fn
          ([type]
             (data/fetch type))
          ([type filters sort-and-page]
             (apply data/fetch (tables/carte-table-adapter type
                                                           filters
                                                           sort-and-page))))
        (= k :lookup)
        (fn [type id]
          (if (= :app_user type)
            (data/fetch-one type {:id id} :with :roles)
            (data/fetch-id type id)))
        
        (= k :delete)
        (fn [type id]
          (if (= :app_user type)
            (let [user (-> (data/fetch-one type {:id id} :with :roles)
                           (assoc :roles []))]
              (do (data/save type user)
                  (data/delete user)))
            (data/delete-id type id)))))

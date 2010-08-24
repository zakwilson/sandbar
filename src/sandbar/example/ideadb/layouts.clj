;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.layouts
  (:require [clojure.contrib.str-utils2 :as s])
  (:use (hiccup core page-helpers)
        (sandbar core
                 [auth :only (current-user
                              current-username
                              any-role-granted?)]
                 stateful-session)))

(defn base-layout [title header request & body]
  (html
   (doctype :html4)
   [:html
    [:head
     [:meta {:http-equiv "X-UA-Compatible" :content "IE=EmulateIE7"}]
     [:title (str "Idea Database - " (s/capitalize title))]
     (icon "icon.png")
     (stylesheet "sandbar.css")
     (stylesheet "ideadb.css")]
    [:body
     [:div {:id "page"}
      header
      [:div {:id "content"}
       (if-let [m (get-flash-value! :user-message)]
         [:div {:class "message"}
          m])
       body]
      [:div {:id "footer"}
       (if (not (nil? (current-user)))
         [:div "You are currently logged in as "
          [:b (current-username)] ". "
          (clink-to "/logout" "logout")])]]
     (map javascript ["ideadb.js" "prototype/prototype.js"])
     #_(map javascript ["ideadb.js" "jquery-1.4.2.min.js"])]]))

(defn main-layout [title request & body]
  (base-layout title [:div] request body))

(defn list-layout [title request & body]
  (base-layout title
               (if (any-role-granted? request :admin)
                 [:div {:id "idea-actions"}
                  (link-to "/idea/new" "Submit and Idea") " | "
                  (link-to "/admin/list" "Edit Lists") " | "
                  (link-to "/idea/download" "Download")]
                 [:div {:id "idea-actions"}
                  (link-to "/idea/new" "Submit and Idea")])
               request
               body))

(defn form-layout [title request & body]
  (base-layout title
               (if (any-role-granted? request :admin)
                 [:div {:id "form-page-header"}]
                 [:div {:id "form-page-header"}
                  (str "Welcome " (:name (current-user))
                       "! Use the form below to submit your idea.")])
               request
               body))

(defn admin-users-layout [title request & body]
  (base-layout title
               [:div {:id "form-page-header"}]
               request
               (if (.endsWith (:uri request) "list")
                 [:div
                  [:div (clink-to "/admin/list" "Return to Edit Lists")]
                  [:br]
                  body]
                 body)))






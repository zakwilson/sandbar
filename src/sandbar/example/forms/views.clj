(ns sandbar.example.forms.views
  "Common views which are used by all of the form examples."
  (:use [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [flash-get]]
        [sandbar.core :only [icon stylesheet]])
  (:require [sandbar.example.forms.database :as db]))

(defn layout [content]
  (html
   (doctype :html4)
   [:html
    [:head
     (stylesheet "sandbar.css")
     (stylesheet "sandbar-forms.css")
     (icon "icon.png")]
    [:body
     (if-let [m (flash-get :user-message)] [:div {:class "message"} m])
     [:h2 "Sandbar Form Example"]
     content]]))

(defn home []
  (layout
   [:div
    (link-to "/user/edit" "Add New User")
    [:table
     [:tr
      [:th "Username"]
      [:th ""]]
     (map #(let [{:keys [username id]} %]
             [:tr
              [:td username]
              [:td (link-to (str "/user/edit/" id) "Edit")]])
          (db/all-things))]]))

(ns sandbar.dev.views
  "Common views which are used by all of the form examples."
  (:use [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [flash-get]]
        [sandbar.core :only [icon stylesheet]])
  (:require [sandbar.example.database :as db]))

(defn layout [body]
  (let [[body title] (if (map? body)
                       ((juxt :body :title) body)
                       [body ""])]
    (html
     (doctype :html4)
     [:html
      [:head
       (stylesheet "sandbar.css")
       (stylesheet "sandbar-forms.css")
       (icon "icon.png")
       [:title title]]
      [:body
       (if-let [m (flash-get :user-message)] [:div {:class "message"} m])
       [:h2 "Sandbar Form Example"]
       body]])))

(defn home []
  (layout
   [:div
    (link-to "/users/new" "Add New User")
    [:table
     [:tr
      [:th "Username"]
      [:th ""]]
     (map #(let [{:keys [username id]} %]
             [:tr
              [:td username]
              [:td (link-to (str "/users/" id "/edit") "Edit")]])
          (db/all-things))]]))

(ns sandbar.example.forms.simple-form-demo
  "Create a very simple form using sandbar.forms. The purpose of this demo is
   to show how easy it is to create a form in the most simple case."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [compojure.core :only [defroutes GET]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         set-flash-value!
                                         get-flash-value]]
        [sandbar.core :only [icon stylesheet]]
        [sandbar.forms :as forms])
  (:require [compojure.route :as route]
            [sandbar.example.forms.database :as db]))

(defn layout [content]
  (html
   (doctype :html4)
   [:html
    [:head
     (stylesheet "sandbar.css")
     (stylesheet "sandbar-forms.css")
     (icon "icon.png")]
    [:body
     (if-let [m (get-flash-value :user-message)] [:div {:class "message"} m])
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
     (map #(let [{:keys [username last-name id]} %]
             [:tr
              [:td username]
              [:td last-name]
              [:td (link-to (str "/user/edit/" id) "Edit")]])
          (db/all-users))]]))

(forms/defform user-form "/user/edit"
  :fields [(forms/hidden :id)
           (forms/textfield "Username" :username)]
  :load #(db/find-user %)
  :on-cancel "/"
  :on-success
  #(do
     (db/store-user %)
     (set-flash-value! :user-message "User has been saved.")
     "/"))

(defroutes routes
  (user-form (fn [_ form] (layout form)))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

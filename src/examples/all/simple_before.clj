(ns examples.all.simple-before
  "Using all aspects of sandbar in one simple application."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware [params :only [wrap-params]]]
        [ring.middleware.file :only [wrap-file]]
        [ring.middleware.reload :only [wrap-reload]]
        [ring.middleware.session.memory :only [memory-store]]
        [compojure.core :only [defroutes GET]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         session-put!
                                         flash-put!
                                         flash-get]]
        [sandbar.forms2]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   add-validation-error]]
        [sandbar.auth :only [current-username
                             any-role-granted?
                             defauth
                             logout!
                             with-secure-channel
                             with-security]]
        [sandbar.form-authentication]
        [sandbar.core :only [icon stylesheet]])
  (:require [compojure.route :as route]
            [examples.forms.database :as db]))

;; Views
;; =====

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
       (if-let [m (flash-get :user-message)] [:div.message m])
       [:h2 "Sandbar Demo"]
       [:p "Maintain a list of users."]
       body]])))

(defn view-home []
  (layout
   [:div
    (link-to "/admin/users/new" "Add New User")
    [:table
     [:tr
      [:th "Username"]
      [:th ""]]
     (map #(let [{:keys [username id]} %]
             [:tr
              [:td username]
              [:td (link-to (str "/users/" id "/view") "View")]
              [:td (link-to (str "/admin/users/" id "/edit") "Edit")]])
          (db/all-things))]]))

(defn view-user [id]
  (layout
   (str (db/fetch id))))

(defn view-permission-denied []
  [:div
   [:h3 "Permission Denied"]
   [:div (link-to "/" "Home")]])

;; End Views
;; =========

(defroutes routes
  (GET "/" [] (view-home))
  (GET "/users/:id/view" [id] (view-user id))
  (GET "/permission-denied*" [] (layout (view-permission-denied)))
  (GET "/logout" []  (logout! {}))
  (route/not-found "<h1>Not Found</h1>"))

(defonce my-session (atom {}))

(def app
     (-> routes
         (wrap-stateful-session {:store (memory-store my-session)})
         (wrap-file "public")
         wrap-params
         (wrap-reload ['examples.all.simple-before])))

(defn run []
  (run-jetty (var app) {:join? false :port 8080}))

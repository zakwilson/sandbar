(ns examples.all.simple
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
       body
       (if (any-role-granted? :user :admin)
         [:p (str "You are logged in as " (current-username) ". ")
          (link-to "/logout" "Logout")])]])))

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

(defform user-form
  :resource (restful-resource "/admin/users" :id)
  :fields [(textfield :username :label "Username" :required true)
           (button :save)
           (button :cancel)]
  :load (fn [id] (db/fetch id))
  :on-cancel "/"
  :on-success (fn [m]
                (db/store m)
                (flash-put! :user-message "User has been saved.")
                "/")
  :validator (build-validator (non-empty-string :username))
  :page-layout (fn [_ body] (layout body)))

(def security-policy
     [#"/admin.*"            [:admin :ssl]
      #"/login.*"            [:any :ssl]
      #".*.(css|png|gif|js)" [:any :any-channel]
      #"/.*"                 [:any :nossl]])

(defauth authorize
  :type :form
  :load (fn [username password]
          (let [login {:username username :password password}]
            (cond (= username "user") (merge login {:roles #{:user}})
                  (= username "admin") (merge login {:roles #{:admin}})
                  :else login)))
  :validator #(if (= (:password %) (:username %))
                %
                (add-validation-error % "Incorrect username or password!")))

(defroutes routes
  
  user-form
  
  (GET "/" [] (view-home))
  (GET "/users/:id/view" [id] (view-user id))
  (GET "/permission-denied*" [] (layout (view-permission-denied)))
  (GET "/logout" []  (logout! {}))
  (authorize (fn [_ body] (layout body)))
  (route/not-found "<h1>Not Found</h1>"))

(defonce my-session (atom {}))

(def app
     (-> routes
         (with-security security-policy authorize)
         (wrap-stateful-session {:store (memory-store my-session)})
         (wrap-file "public")
         wrap-params
         (with-secure-channel security-policy 8080 8443)
         (wrap-reload ['examples.all.simple])))

(defn run []
  (run-jetty (var app) {:join? false :ssl? true :port 8080 :ssl-port 8443
                        :keystore "my.keystore"
                        :key-password "foobar"}))

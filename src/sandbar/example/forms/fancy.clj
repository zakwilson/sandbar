(ns sandbar.example.forms.fancy
  "Fancy form with help from jQuery."
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [hiccup.core :only [html]]
        [hiccup.page-helpers :only [doctype link-to]]
        [compojure.core :only [defroutes GET]]
        [sandbar.core :only [icon stylesheet javascript]]
        [sandbar.stateful-session :only [wrap-stateful-session
                                         flash-put!
                                         flash-get]]
        [sandbar.validation :only [add-validation-error
                                   build-validator
                                   non-empty-string]])
  (:require [compojure.route :as route]
            [sandbar.forms :as forms]
            [sandbar.example.forms.database :as db]
            [clojure.contrib.json :as json]
            [net.cgrand.enlive-html :as enlive]))

(defn layout
  ([content] (layout content []))
  ([content javascripts]
     (html
      (doctype :html5)
      [:html
       [:head
        (stylesheet "sandbar.css")
        (stylesheet "sandbar-forms.css")
        (stylesheet "fancy-form.css")
        (stylesheet "ui-lightness/jquery-ui-1.8.4.custom.css")
        (icon "icon.png")]
       [:body
        (if-let [m (flash-get :user-message)] [:div {:class "message"} m])
        [:h2 "Sandbar Form Example"]
        content
        (map javascript
             (concat ["jquery-1.4.2.min.js" "jquery-ui-1.8.4.custom.min.js"]
                     javascripts))]])))

(defn home []
  (layout
   [:div
    (link-to "/developer/edit" "Add Developer")
    [:table
     [:tr
      [:th "Name"] [:th "Hire Date"] [:th "Favorite Language"] [:th ""]]
     (map #(let [{:keys [id name hire-date language]} %]
             [:tr
              [:td name] [:td hire-date] [:td language]
              [:td (link-to (str "/developer/edit/" id) "Edit")]])
          (db/all-things))]]))

(def properties {:name "Name"
                 :hire-date "Hire Date"
                 :language "Language"})

(defn get-langs-from-github []
  (let [page (-> "http://github.com/languages/"
                 java.net.URL.
                 enlive/html-resource)
        hits (enlive/select page [:div#languages :li :a])]
    (mapcat #(:content %) hits)))

(def langs (memoize get-langs-from-github))

(defn langs-starting-with [{params :params}]
  (let [term (get params "term")
        pattern (re-pattern (str "^" term ".*"))
        langs (filter #(re-find pattern %) (langs))]
    (json/json-str (map #(hash-map :value %) langs))))

(forms/defform fancy-form "/developer/edit"
  :fields [(forms/hidden :id)
           (forms/textfield :name {:id :name})
           (forms/textfield :hire-date {:id :hire-date :size 10})
           (forms/textfield :language {:id :language})]
  :load #(db/fetch %)
  :on-cancel "/"
  :on-success #(do
                 (db/store %)
                 (flash-put! :user-message "Developer has been saved.")
                 "/")
  :validator #(non-empty-string % :name :hire-date :language properties)
  :properties properties
  :ajax [langs-starting-with]
  :ajax-validation-at "validate")

(defroutes routes
  (fancy-form (fn [request form]
                (layout form ["fancy-form.js"])))
  (GET "/" [] (home))
  (route/not-found "<h1>Not Found</h1>"))

(def application-routes (-> routes
                            wrap-stateful-session
                            (wrap-file "public")))

(defn run []
  (run-jetty (var application-routes) {:join? false :port 8080}))

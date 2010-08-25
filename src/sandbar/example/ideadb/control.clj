;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.control
  (:use (compojure core)
        (ring.util [response :only (redirect)])
        (sandbar auth
                 [core :only (cpath
                              clink-to
                              property-lookup)]
                 util
                 form-authentication)
        (sandbar.dev user-manager
                     standard-pages
                     list-manager
                     autorouter)
        (sandbar.example.ideadb properties
                                layouts
                                users))
  (:require (sandbar.example.ideadb [data :as data]
                                    [ideas :as ideas])))

;;
;; Views
;; =====
;;

(defn admin-menu-view [request]
  (let [links {"business-unit" "Edit Business Units"
               "idea-category" "Edit Categories"
               "idea-status" "Edit Status List"
               "idea-type" "Edit Types"}]
    [:div
     [:div (clink-to "/ideas" "Return to Idea list")]
     [:br]
     [:div (clink-to "/admin/user/list" "Edit Users")]
     [:br]
     (map #(vector :div (clink-to (str "/admin/" (key %) "/list") (val %)))
         links)]))

;;
;; Control
;; =======
;;

(defn index [request]
  (redirect "/ideas"))

(defn permission-denied [request]
  (main-layout "Permission Denied"
               request
               (permission-denied-page)))

;;
;; Routes
;; ======
;;

(defn route-adapter
  "Adapt the routing algorithm to this project."
  [c a]
  (if c
    (let [ctrl (if (= c "idea") "ideas" c)]
        (cond (or (= a "list") (= a "download"))
           [ctrl (str c "-" a)] 
           (or (= a "new")
               (= a "edit")
               (= a "delete"))
           [ctrl (str a "-" c)]))
    (if (= a "ideas")
      ["ideas" "idea-list"]
      [c a])))

;; Build json support into the autorouter. The autorouter will route
;; post requests to the correct location. You need a way to cause the
;; reqpose to be json.

(defroutes ideadb-routes
  (binding [*reload-namespaces* true]
      (autorouter route-adapter))
  
  (GET "/admin/list*" request
       (main-layout "Administrator"
                    request
                    (admin-menu-view request)))
  
  (apply routes
         (map #(list-manager-routes
                (fn [r b]
                  (main-layout (str "Edit "
                                    (property-lookup properties  %)
                                    " List")
                               r
                               b))
                "/admin"
                (cpath "/admin/list")
                (data/simple-list % properties))
              [:business_unit :idea_category :idea_status :idea_type]))
  
  (security-edit-user-routes "/admin" (var admin-users-layout) (fn [r] (:uri r))
                             properties user-data-functions)
  
  (form-authentication-routes (fn [r & b] (main-layout "Login" r b)) 
                     (form-authentication-adapter (user-data-functions :load)
                                         properties)))

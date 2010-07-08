;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.javascripts
  (:require (sandbar.dev [tables :as tables])))

(defn wrap-sandbar-js
  ([handler]
     (wrap-sandbar-js handler "/js"))
  ([handler prefix]
     (fn [request]
       (let [uri (:uri request)]
         (if (= uri (str prefix "/tables.js"))
           {:status 200
            :headers {"Content-Type" "text/javascript"}
            :body (tables/js)} 
           (handler request))))))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.stateful-session
  "Middleware for working with 'stateful' sessions."
  (:use (ring.middleware flash session)))

(declare sandbar-session)
(declare sandbar-flash)

(defn wrap-stateful-session*
  "Add stateful sessions to a ring handler. Does not modify the functional
   behavior of ring sessions except that returning nil will not remove
   the session if you have stateful data. Creates a separate namespace
   for stateful session keys so that user code and library code will not
   interfere with one another. Also adds map style flash support backed by
   Ring's flash middleware."
  [handler]
  (fn [request]
    (binding [sandbar-session (atom (-> request :session ::session))
              sandbar-flash (atom {:incoming (-> request :flash)})]
      (let [request (update-in request [:session] dissoc ::session)
            response (handler request)
            sandbar-session @sandbar-session
            outgoing-flash (merge (:outgoing @sandbar-flash)
                                  (:flash response))
            sandbar-session (if (empty? sandbar-session)
                              nil
                              sandbar-session)
            request-session (dissoc (:session request) ::session)
            response-session (:session response)
            session  (if (contains? response :session)
                       (or response-session {})
                       request-session) 
            session (if sandbar-session
                      (assoc session ::session sandbar-session)
                      session)]
        (when response
          (let [response (if (nil? session)
                           (dissoc response :session)
                           (if (empty? session)
                             (merge response {:session nil})
                             (merge response {:session session})))]
            (if outgoing-flash
              (assoc response :flash outgoing-flash)
              response)))))))

(defn wrap-stateful-session
  ([handler]
     (wrap-stateful-session handler {}))
  ([handler options]
     (-> handler
         wrap-stateful-session*
         wrap-flash
         (wrap-session options))))

(defn update-session! [update-fn value]
  (swap! sandbar-session update-fn value))

(defn session-put! [k v]
  (swap! sandbar-session (fn [a b] (merge a {k b})) v))

(defn session-get
  ([k] (session-get k nil))
  ([k default] (if (vector? k)
                 (get-in @sandbar-session k)
                 (get @sandbar-session k default))))

(defn session-delete-key! [k]
  (swap! sandbar-session (fn [a b] (dissoc a b)) k))

(defn destroy-session! []
  (swap! sandbar-session (constantly nil)))

(defn flash-put!
  "Add a value to the flash in such a way that it is available in both
   this request and the next."
  [k v]
  (swap! sandbar-flash (fn [a b] (-> a
                                     (assoc-in [:outgoing k] b)
                                     (assoc-in [:incoming k] b))) v))

(defn flash-get
  "Get a value from the flash which may have been added during the current or
   previous request."
  [k]
  (try (-> @sandbar-flash
           :incoming
           k)
       (catch Exception _ nil)))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns testdrive
  "Provide an easy way for developers to setup and run the reference
   application."
  (:require [sandbar.example.ideadb.migrations :as ideadb-config]
            [sandbar.dev.user-tables-mysql :as user-config]
            [carte.core :as database]))

(def testdrive-db (user-config/get-connection))

(try (database/fetch testdrive-db :idea)
     (catch java.lang.Exception name
      (do (println "Idea Database tables do not exist. Creating them now.")
          (ideadb-config/create-tables))))

(try (database/fetch testdrive-db :app_user)
     (catch java.lang.Exception name
      (do (println "User tables do not exist. Creating them now.")
        (user-config/create-tables))))

(use 'sandbar.example.ideadb.app)
(run)

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.forms2.example-basic
  (:use [sandbar.forms2]))

(def example-field
     (textfield :name))

(def example-form-1
     (form [(textfield :name)]))

(def example-form-2
     (form [(textfield :name)]
           :create-action "/names"
           :update-action "/names/:id"
           :update-method :put
           :layout (grid-layout)))

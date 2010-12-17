;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test.forms2
  (:use [clojure.test :only [deftest testing is]]
        [sandbar.forms2])
  (:require [net.cgrand.enlive-html :as html]))

;; ============
;; Test Helpers
;; ============

(defn <- [s] (html/html-resource (java.io.StringReader. s)))

(defn field-cell [t id]
  (let [field (-> (html/select t [id]) first :attrs :class keyword)
        cell (first
              (filter #(and (= (:tag %) :div)
                            (= (-> % :attrs :class) "field-cell"))
                      (html/select t [(html/has [id])])))]
    {:type field
     :cell cell}))

(defn cell-type-dispatch [m] (:type m))

(defmulti label* cell-type-dispatch)

(defmethod label* :default [m]
           (html/text (first (html/select (:cell m) [:div.field-label]))))

(defmulti required?* cell-type-dispatch)

(defmethod required?* :default [m]
           (not
            (empty?
             (html/select (:cell m) [:div.field-label :span.required]))))

(defmulti error-message* cell-type-dispatch)

(defmethod error-message* :default [m]
           (html/text
            (first
             (html/select (:cell m) [:div.field-error-message]))))

(defmulti error-visible?* cell-type-dispatch)

(defmethod error-visible?* :default [m]
           (not (-> (:cell m)
                    (html/select [:div.field-error-message])
                    first
                    :attrs
                    :style
                    (= "display:none;"))))

(defn id-selector [id]
  (keyword (str "#" (name id))))

(defn label [h id]
  (let [template (<- h)
        cell (field-cell template (id-selector id))]
    (label* cell)))

(defn attr [h id attr]
  (let [template (<- h)]
    (-> template
        (html/select [(id-selector id)])
        first
        :attrs
        attr)))

(defn required? [h id]
  (let [template (<- h)
        cell (field-cell template (id-selector id))]
    (required?* cell)))

(defn error-message [h id]
  (let [template (<- h)
        cell (field-cell template (id-selector id))]
    (error-message* cell)))

(defn error-visible? [h id]
  (let [template (<- h)
        cell (field-cell template (id-selector id))]
    (error-visible?* cell)))

(defn form-attrs [h id]
  (let [template (<- h)]
    (-> template
        (html/select [(id-selector id)])
        first
        :attrs)))

(defn exists? [h id]
  (let [template (<- h)]
    (not (-> template
             (html/select [(id-selector id)])
             empty?))))

(defn form-action [h id]
  (:action (form-attrs h id)))

(defn form-method [h id]
  (:method (form-attrs h id)))

(defn fields-and-vals [h id]
  (let [template (<- h)
        form (-> template
                 (html/select [(id-selector id)])
                 first)]
    (apply hash-map (flatten
                     (->> (html/select form [:input])
                          (map #(-> % :attrs))
                          (map #(vector (keyword (:name %)) (:value %))))))))

;; =====
;; Tests
;; =====

(deftest textfield-tests
  (let [h (render-field (textfield :name :id :name-) {} {})]
    (is (= (label h :name-) ""))
    (is (= (attr h :name- :size) "35"))
    (is (= (attr h :name- :name) "name"))
    (is (false? (required? h :name-)))
    (is (false? (error-visible? h :name-))))
  (let [field (textfield :name :id :name- :label "Name")]
    (let [h (render-field field {} {})]
      (is (= (label h :name-) "Name")))
    (let [h (render-field field {:name "a"} {})]
      (is (= (attr h :name- :value) "a")))
    (let [h (render-field field {:name "a"} {:errors {:name ["r"]}})]
      (is (= (attr h :name- :style) nil))
      (is (= (error-message h :name-) "r")))
    (let [h (render-field (textfield :name
                                     :id :name-
                                     :label "Name"
                                     :required true)
                          {:name "a"} {:errors {:name ["name error"]}})]
      (is (= (attr h :name- :value) "a"))
      (is (true? (required? h :name-)))
      (is (true? (error-visible? h :name-)))
      (is (= (error-message h :name-) "name error")))))

(deftest form-tests
  (let [request {:uri "/a"}
        fields [(textfield :name
                           :id :name-
                           :label "My Name")
                (button :submit)
                (button :cancel)]]
    (let [form (form :user-form
                     :id :user-form)]
      (let [h (:body (render-form form request fields {} {}))]
        (is (= (form-action h :user-form) "/a"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name ""
                :submit "Submit"
                :cancel "Cancel"})))
      (let [h (:body (render-form form request fields {:name "b"} {}))]
        (is (= (fields-and-vals h :user-form)
               {:name "b"
                :submit "Submit"
                :cancel "Cancel"}))))
    (let [form (form :user-form
                     :create-action "/users"
                     :update-action "/users/:id"
                     :update-method :put
                     :id :user-form)]
      (let [h (:body (render-form form request fields {} {}))]
        (is (= (form-action h :user-form) "/users"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name ""
                :submit "Submit"
                :cancel "Cancel"})))
      (let [h (:body (render-form form
                                  (assoc request :route-params {"id" 7})
                                  fields
                                  {:name "x"}
                                  {}))]
        (is (= (form-action h :user-form) "/users/7"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name "x"
                :_method "PUT"
                :submit "Submit"
                :cancel "Cancel"}))))
    (let [form (form :user-form
                     :layout (grid-layout :title "My Title"))]
      (let [{:keys [body title]} (render-form form request fields {} {})]
        (is (= title "My Title"))))))

(deftest embedded-form-test
  (let [form (form :user-form
                   :create-action "/users"
                   :update-action "/users/:id"
                   :update-method :put
                   :id :user-form
                   :layout (grid-layout :title "My Title"))]
    (testing "embedded forms"
      (let [request {:uri "/a"}
            fields [(textfield :name :id :name- :label "My Name")]]
        (let [ef (embedded-form form fields)
              {:keys [title body]} (process-request ef request)]
          (is (= title "My Title"))
          (is (= (form-action body :user-form) "/users"))
          (is (= (form-method body :user-form) "POST"))
          (is (= (fields-and-vals body :user-form)
                 {:name ""})))
        (testing "get defaults"
          (let [ef (embedded-form form
                                  fields
                                  :defaults {:name "x"})
                {:keys [title body errors]} (process-request ef request)]
            (is (= (attr body :name- :value) "x"))
            (is (nil? errors))
            (is (= title "My Title"))))
        (testing "loads data"
          (let [ef (embedded-form form
                                  fields
                                  :data-source {:name "y"}
                                  :defaults {:name "x"})
                {:keys [title body errors]} (process-request ef request)]
            (is (= (attr body :name- :value) "y"))
            (is (nil? errors))
            (is (= title "My Title"))))
        (testing "get errors and input"
          (let [ef (embedded-form form
                                  fields
                                  :temp-store {:errors {:name ["name err"]}
                                               :data {:name "z"}}
                                  :data-source {:name "y"}
                                  :defaults {:name "x"})
                {:keys [title body errors]} (process-request ef request)]
            (is (= (attr body :name- :value) "z"))
            (is (= errors {:name ["name err"]}))
            (is (= title "My Title"))
            (is (true? (error-visible? body :name-)))
            (is (= (error-message body :name-) "name err")))))
      (testing "fields as a function"
        (let [fields (fn [request]
                       (if (= (-> request :uri) "/a")
                         [(textfield :name :id :name- :label "My Name")]
                         [(textfield :name :id :name- :label "My Name")
                          (textfield :age :id :age- :label "My Age")]))]
          (let [ef (embedded-form form fields)
                {:keys [title body]} (process-request ef {:uri "/a"})]
            (is (= title "My Title"))
            (is (true? (exists? body :name-)))
            (is (false? (exists? body :age-))))
          (let [ef (embedded-form form fields)
                {:keys [title body]} (process-request ef {:uri "/b"})]
            (is (= title "My Title"))
            (is (true? (exists? body :name-)))
            (is (true? (exists? body :age-)))))))))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test.forms2
  (:use [clojure.test :only [deftest testing is]]
        [sandbar.core :only [get-param]]
        [sandbar.forms2]
        [sandbar.validation :only [build-validator
                                   non-empty-string]])
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

(defrecord TestResponse [] SubmitResponse
  (canceled [this form-info]
            {:type :canceled
             :form-info form-info})
  (failure [this form-info]
           {:type :failure
            :form-info form-info})
  (success [this form-info]
           {:type :success
            :form-info form-info}))

(defn test-response []
  (TestResponse.))

(defrecord TestResource []
  Resource
  (new-uri [this] "/new")
  (create-uri [this] "/create")
  (edit-uri [this] "/edit")
  (update-uri [this] "/update")
  (resource-id [this request] (get-param (:params request) :id))
  (submit-method [this request] :post)
  (submit-action [this request] "/submit"))

(defn test-resource []
  (TestResource.))

;; =====
;; Tests
;; =====

(deftest add-errors-tests
  (let [form (form :user-form)
        form-info {:request
                   {:flash
                    {:user-form {:errors {:name ["name err"]}}}}}
        processor (add-errors form)]
    (is (= (:errors (processor form-info))
           {:name ["name err"]}))
    (is (= (processor {})
           {:errors nil}))))

(deftest add-previous-input-tests
  (let [form (form :user-form)
        form-info {:errors {:name ["form err"]}
                   :request
                   {:flash
                    {:user-form {:data {:name "x"}}}}}
        processor (add-previous-input form)]
    (is (= (processor form-info)
           (merge form-info {:form-data {:name "x"}})))
    (is (= (processor (dissoc form-info :errors))
           (dissoc form-info :errors)))))

(deftest add-source-tests
  (let [resource (test-resource)]
    (testing "load source"
      (let [processor (add-source resource identity)]
        (testing "load is called"
          (let [form-info {:request {:params {:id 42}}}]
            (is (= (:form-data (processor form-info))
                   42))))
        (testing "load is not called when data is present"
          (let [request {:params {"id" "42"}}
                form-info {:form-data {:name "x"}
                           :request request}]
            (is (= (:form-data (processor form-info))
                   {:name "x"}))))
        (testing "returns nil when no data is loaded"
          (let [form-info {:request {}}]
            (is (nil? (:form-data (processor form-info))))))))))

(deftest add-defaults-tests
  (testing "defaults as map"
    (let [processor (add-defaults {:name "z"})]
      (testing "when form data exists"
        (let [form-info {:form-data {:name "x"}}]
          (is (= (:form-data (processor form-info))
                 {:name "x"}))))
      (testing "when form data does not exist"
        (let [form-info {}]
          (is (= (:form-data (processor form-info))
                 {:name "z"}))))))
  (testing "defaults as function"
    (let [processor (add-defaults (fn [request] {:name "z"}))]
      (testing "when form data does not exist"
        (let [form-info {}]
          (is (= (:form-data (processor form-info))
                 {:name "z"})))))))

(deftest add-cancel-tests
  (let [request {}
        fields [(textfield :name :id :name- :label "My Name")
                (button :submit)
                (button :cancel)]
        ac (add-cancel)
        form-info {:fields fields}
        {:keys [fields]} (ac form-info)]
    (is (= (count (filter #(= (:type (field-map %)) :hidden) fields))
           1))))

(deftest form-view-tests
  (testing "form view"
    (testing "without processors"
      (let [fields []
            processor identity]
        (is (= (form-view fields processor {})
               {:form-data nil
                :params {}
                :errors nil
                :request {}
                :response {}
                :fields fields
                :i18n {}}))
        (let [request {:params {"name" "x" "id" "42"}}]
          (is (= (form-view fields processor request)
                 {:form-data nil
                  :params {:name "x"
                           :id 42}
                  :errors nil
                  :request request
                  :response {}
                  :fields fields
                  :i18n {}})))))
    (testing "with processors"
      (let [form (form :user-form)
            resource (test-resource)
            fields [(textfield :name :id :name- :label "My Name")
                    (button :submit)
                    (button :cancel)]
            processors [(add-errors form)
                        (add-previous-input form)
                        (add-source resource
                                    (fn [id]
                                      {:name "y" :id id}))
                        (add-defaults {:name "z"})]
            view-processor (apply comp (reverse processors))]
        (testing "error"
          (let [request {:flash
                         {:user-form {:data {:name "x"}
                                      :errors {:name ["name err"]}}}}
                result (form-view fields view-processor request)]
            (is (= (select-keys result [:form-data :errors])
                   {:form-data {:name "x"}
                    :errors {:name ["name err"]}}))))
        (testing "edit"
          (let [params {"id" "42"}
                request {:flash {} :params params}
                result (form-view fields view-processor request)]
            (is (= (select-keys result [:form-data :params])
                   {:form-data {:name "y" :id 42}
                    :params {:id 42}}))))
        (testing "create"
          (let [request {}
                result (form-view fields view-processor request)]
            (is (= (select-keys result [:form-data])
                   {:form-data {:name "z"}}))))))))

;;
;; Form Fields
;;

(deftest textfield-tests
  (let [h (render (textfield :name :id :name-) {})]
    (is (= (label h :name-) ""))
    (is (= (attr h :name- :size) "35"))
    (is (= (attr h :name- :name) "name"))
    (is (false? (required? h :name-)))
    (is (false? (error-visible? h :name-))))
  (let [field (textfield :name :id :name- :label "Name")]
    (let [h (render field {})]
      (is (= (label h :name-) "Name")))
    (let [h (render field {:form-data {:name "a"}})]
      (is (= (attr h :name- :value) "a")))
    (let [h (render field {:form-data {:name "a"}
                                 :errors {:name ["r"]}})]
      (is (= (attr h :name- :style) nil))
      (is (= (error-message h :name-) "r")))
    (let [h (render (textfield :name
                                     :id :name-
                                     :label "Name"
                                     :required true)
                          {:form-data {:name "a"}
                           :errors {:name ["name error"]}})]
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
                (button :cancel)]
        resource (test-resource)
        form-info {:request request
                   :fields fields}]
    (let [form (form :user-form
                     :resource resource
                     :id :user-form)]
      (let [h (render form form-info)]
        (is (= (form-action h :user-form) "/submit"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name ""
                :submit "Submit"
                :cancel "Cancel"})))
      (let [h (render form (assoc form-info :form-data {:name "b"}))]
        (is (= (fields-and-vals h :user-form)
               {:name "b"
                :submit "Submit"
                :cancel "Cancel"}))))
    (let [resource (restful-resource "/users" :id)
          form (form :user-form
                     :resource resource
                     :id :user-form)]
      (let [h (render form form-info)]
        (is (= (form-action h :user-form) "/users"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name ""
                :submit "Submit"
                :cancel "Cancel"})))
      (let [h (render form
                      {:request (assoc request :route-params {"id" 7})
                       :form-data {:name "x"}
                       :fields fields})]
        (is (= (form-action h :user-form) "/users/7"))
        (is (= (form-method h :user-form) "POST"))
        (is (= (fields-and-vals h :user-form)
               {:name "x"
                :_method "PUT"
                :submit "Submit"
                :cancel "Cancel"}))))))

(deftest embedded-form-test
  (let [resource (restful-resource "/users" :id)
        form (form :user-form
                   :resource resource
                   :id :user-form
                   :layout (grid-layout))]
    (testing "embedded forms"
      (let [request {:uri "/a"}
            fields [(textfield :name :id :name- :label "My Name")]]
        (let [ef (embedded-form form resource fields)
              {:keys [response]} (process-request ef request)
              body (:body response)]
          (is (= (form-action body :user-form) "/users"))
          (is (= (form-method body :user-form) "POST"))
          (is (= (fields-and-vals body :user-form)
                 {:name ""})))
        (testing "get defaults with map"
          (let [ef (embedded-form form
                                  resource
                                  fields
                                  :defaults {:name "x"})
                {:keys [response errors]} (process-request ef request)
                body (:body response)]
            (is (= (attr body :name- :value) "x"))
            (is (nil? errors))))
        (testing "get defaults with function"
          (let [ef (embedded-form form
                                  resource
                                  fields
                                  :defaults (fn [req] {:name "x"}))
                {:keys [response errors]} (process-request ef request)
                body (:body response)]
            (is (= (attr body :name- :value) "x"))
            (is (nil? errors))))
        (testing "loads data"
          (let [request (merge request {:route-params {:id 3}})
                ef (embedded-form form
                                  resource
                                  fields
                                  :load (fn [_] {:name "y"})
                                  :defaults {:name "x"})
                {:keys [response errors]} (process-request ef request)
                body (:body response)]
            (is (= (attr body :name- :value) "y"))
            (is (nil? errors))))
        (testing "get errors and input"
          (let [request {:flash {:user-form
                                 {:errors {:name ["name err"]}
                                  :data {:name "z"}}}}
                ef (embedded-form form
                                  resource
                                  fields
                                  :load (fn [params] {:name "y"})
                                  :defaults {:name "x"})
                {:keys [response errors]} (process-request ef request)
                body (:body response)]
            (is (= (attr body :name- :value) "z"))
            (is (= errors {:name ["name err"]}))
            (is (true? (error-visible? body :name-)))
            (is (= (error-message body :name-) "name err")))))
      (testing "fields as a function"
        (let [fields (fn [request]
                       (if (= (-> request :uri) "/a")
                         [(textfield :name :id :name- :label "My Name")]
                         [(textfield :name :id :name- :label "My Name")
                          (textfield :age :id :age- :label "My Age")]))]
          (let [ef (embedded-form form resource fields)
                {{body :body} :response} (process-request ef {:uri "/a"})]
            (is (true? (exists? body :name-)))
            (is (false? (exists? body :age-))))
          (let [ef (embedded-form form resource fields)
                {{body :body} :response} (process-request ef {:uri "/b"})]
            (is (true? (exists? body :name-)))
            (is (true? (exists? body :age-))))))
      (testing "cancel control"
        (let [fields [(textfield :name :id :name- :label "My Name")
                      (button :submit)
                      (button :cancel)]]
          (let [ef (embedded-form form resource fields)
                {{body :body} :response} (process-request ef {:uri "/a"})]
            (is (= (fields-and-vals body :user-form)
                   {:name ""
                    :cancel "Cancel"
                    :submit "Submit"
                    :_cancel "cancel"}))))))))

;; =================
;; Submit Processing
;; =================

(deftest validate-tests
  (let [validator (validator-function
                   (build-validator (non-empty-string :name)))
        test-fn #(-> (process-submit validator
                                     (test-response)
                                     {:form-data %})
                     :errors)]
    (is (= (test-fn {})
           {:name ["name cannot be blank!"]}))
    (is (= (test-fn {:name ""})
           {:name ["name cannot be blank!"]}))
    (is (= (test-fn {:name "x"})
           nil))))

(deftest submit-form-tests
  (testing "form submission"
    (testing "success"
      (let [handler (submit-handler [] (test-response))
            request {:params {"name" "x"
                              "submit" "Submit"
                              "_cancel" "cancel"}}
            result (process-request handler request)]
        (is (= (:type result) :success))
        (is (= (-> result :form-info :form-data)
               {:name "x"
                :submit "Submit"} ))))
    (testing "canceled"
      (let [handler (submit-handler [] (test-response))
            result (process-request handler {:params {"name" "x"
                                                      "cancel" "Cancel"
                                                      "_cancel" "cancel"}})]
        (is (= (:type result) :canceled))))
    (testing "validation failure with default impl"
      (let [handler (submit-handler [] (test-response)
                                    :validator
                                    (build-validator (non-empty-string :name)))
            result (process-request handler {:params {"submit" "Submit"
                                                      "_cancel" "cancel"}})]
        (is (= (:type result) :failure))
        (is (= (-> result :form-info :form-data) {:submit "Submit"}))
        (is (= (-> result :form-info :errors)
               {:name ["name cannot be blank!"]}))))
    (testing "validation failure with custom impl"
      (let [handler (submit-handler [] (test-response)
                                    :validator
                                    (validator-function
                                     (build-validator
                                      (non-empty-string :name))))
            result (process-request handler {:params {"submit" "Submit"
                                                      "_cancel" "cancel"}})]
        (is (= (:type result) :failure))
        (is (= (-> result :form-info :form-data) {:submit "Submit"}))
        (is (= (-> result :form-info :errors)
               {:name ["name cannot be blank!"]}))))
    (testing "cancel shortcuts validation"
      (let [handler (submit-handler [] (test-response)
                                    :validator
                                    (build-validator (non-empty-string :name)))
            result (process-request handler {:params {"name" "x"
                                                      "cancel" "Cancel"
                                                      "_cancel" "cancel"}})]
        (is (= (:type result) :canceled))))))

;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Brenton Ashworth"}
  sandbar.forms3
  "Form layout and processing. A simple implementation with fewer protocols
  than forms2."
  (:use [sandbar.core :only [get-param]]
        [sandbar.validation :only [validation-errors]])
  (:require [hiccup.core :as html]
            [clojure.string :as string]))

;; ==============
;; Form Protocols
;; ==============

(defprotocol Html
  (render [this form-info] "Return rendered HTML."))

(defprotocol Field
  (field-map [this] [this data]
             "Return a map of the field's :type, :name and :value."))

(defprotocol Form
  (unique-id [this] "Return a unique identifier for this form."))

(defprotocol SubmitResponse
  (canceled [this form-info])
  (failure [this form-info])
  (success [this form-info]))

(defprotocol SubmitProcessor
  (process-submit [this response form-info]))
  
(defprotocol FormHandler
  (process-request [this request]
                   "Return a map containing :body and other optional keys."))


;; =======================
;; Default Implementations
;; =======================

(defn field-type-dispatch
  [field-map]
  (:type field-map))

(defmulti field-label
  "Create a field label for a form element."
  field-type-dispatch)

(defmethod field-label :default
  [{:keys [label required]}]
  (let [div [:div.field-label label]]
    (if required
      (vec (conj div [:span.required "*"]))
      div)))

(defmulti form-field-cell
  "Create a field cell which includes the field, field label and optionally
  on error message."
  field-type-dispatch)

(defmethod form-field-cell :default
  [{:keys [id html errors] :as field-map}]
  (let [error-message (first errors) ;; TODO show all errors
        error-attrs (if error-message
                      {}
                      {:style "display:none;"})]
    [:div.field-cell
     (field-label field-map)
     [:div.field-error-message error-attrs error-message]
     html]))

(defrecord Textfield [field-name attributes]
  Field
  (field-map [this] (field-map this {}))
  (field-map [_ data] {:type :textfield
                       :name field-name
                       :value (or (field-name data) "")})
  Html
  (render [this {:keys [form-data i18n errors]}]
    (let [d (merge (field-map this form-data)
                   {:label (or (:label this)
                               (get i18n field-name))
                    :errors (get errors field-name)
                    :required (:required this)
                    :id (:id attributes)})
          field-html [:input
                      (merge {:type "text"
                              :name (name field-name)
                              :value (:value d)
                              :class "textfield"}
                             attributes)]]
      (html/html (form-field-cell (assoc d :html field-html))))))

(defrecord Hidden [field-name value]
  Field
  (field-map [this] (field-map this {}))
  (field-map [_ data] {:type :hidden
                       :name field-name
                       :value (or (get data field-name)
                                  value
                                  "")})
  Html
  (render [this {:keys [form-data]}]
    (html/html [:input {:type "hidden"
                        :name (name field-name)
                        :value (:value (field-map this form-data))}])))

(defrecord Button [field-name value]
  Field
  (field-map [_] {:type (if (= field-name :cancel)
                          :cancel-button
                          :submit-button)
                  :name field-name
                  :value value})
  (field-map [this _] (field-map this))
  Html
  (render [_ _]
    (html/html [:input.sandbar-button
                {:type "submit"
                 :name (name field-name)
                 :value value}])))

;;
;; Form layout
;;

(defn button? [field]
  (contains? #{:cancel-button :submit-button} (:type (field-map field))))

(defrecord GridLayout []
  Html
  (render [_ {:keys [request response fields title] :as form-info}]
    (let [buttons (filter button? fields)
          fields (filter (complement button?) fields)
          rendered-fields (map #(render % form-info) fields)
          rendered-buttons (map #(render % form-info) buttons)
          title (if (fn? title)
                  (title request)
                  title)]
      (html/html [:table
                           [:tr
                            [:td
                             [:div rendered-fields]
                             [:div.buttons
                              [:span.basic-buttons rendered-buttons]]]]]))))

;;
;; Form
;;

(defrecord SandbarForm [form-name action-method layout attributes]
  Form
  (unique-id [_] form-name)
  Html
  (render [_ {:keys [request] :as form-info}]
    (let [[action method] (action-method request)
          layout (render layout form-info)
          method-str (.toUpperCase (name method))]
      (html/html
       [:div.sandbar-form
        (-> (if (contains? #{:get :post} method)
              [:form (merge {:method method-str
                             :action action}
                            attributes)]
              [:form (merge {:method "POST" :action action} attributes)
               [:input {:type "hidden"
                        :name "_method"
                        :value method-str}]])
            (conj layout)
            (vec))]))))

(defrecord EmbeddedFormHandler [f]
  FormHandler
  (process-request [_ request] (f request)))

;; ============
;; Constructors
;; ============

;;
;; Form View Processors
;;

(defn add-errors [form]
  (fn [form-info]
    (let [id (unique-id form)
          errors (-> form-info :request :flash id :errors)]
      (assoc form-info :errors errors))))

(defn add-previous-input [form]
  (fn [form-info]
    (let [id (unique-id form)
          errors (-> form-info :errors)]
      (if errors
        (assoc form-info :form-data (-> form-info :request :flash id :data))
        form-info))))

(defn add-source [load-fn]
  (fn [form-info]
    (if-let [form-data (:form-data form-info)]
      form-info
      (assoc form-info :form-data (if-let [params (:params form-info)]
                                    (load-fn params))))))

(defn add-defaults [m]
  (fn [form-info]
    (let [d (if (fn? m)
              (m (:request form-info))
              m)]
      (if-let [form-data (:form-data form-info)]
        form-info
        (assoc form-info :form-data d)))))

(defn render-form [form]
  (fn [form-info]
    (assoc-in form-info [:response :body]
              (render form form-info))))

(defn add-cancel []
  (fn [{:keys [fields] :as form-info}]
    (let [fields
          (if-let [cancel-buttons (->> fields
                                       (filter button?)
                                       (map field-map)
                                       (filter #(= (:type %) :cancel-button)))]
            (let [h (vec (map #(Hidden. :_cancel (:name %))
                              cancel-buttons))]
              (vec (concat fields h)))
            fields)]
      (assoc form-info :fields fields))))

;;
;; Record Constructors
;;

(defn textfield
  "Create a form textfield. The first argument is the field name. Optional
  named arguments are label and required. Any other named arguments will be
  added to the field's html attributes.

  Examples:

  (textfield :age)
  (textfield :age :label \"Age\")
  (textfield :age :label \"Age\" :required true)
  (textfield :age :label \"Age\" :required true :size 50)
  (textfield :age :size 50 :id :age :value \"x\")"
  [field-name & {:keys [label required] :as options}]
  (let [attributes (-> (merge {:size 35} options)
                       (dissoc :label :required))
        field (Textfield. field-name attributes)
        field (if label (assoc field :label label) field)
        field (if required (assoc field :required required) field)]
    field))

(defn button [type & {:keys [label] :as options}]
  (let [label (or label (case type
                              :save "Save"
                              :cancel "Cancel"
                              "Submit"))]
    (Button. type label)))

(defn grid-layout
  "This will implement all of the features of the current grid layout."
  [& {:keys [] :as options}]
  (GridLayout.))

(defn replace-params
  "Replace all routes params by values contained in the given params map."
  [m s]
  (reduce #(string/replace-first %1
                                 (str ":" (first %2))
                                 (second %2))
          s m))

(defn form
  "Create a form..."
  [form-name & {:keys [create-method update-method create-action
                       update-action layout]
                :as options}]
  (let [attributes (dissoc options
                           :create-method :update-method :create-action
                           :update-action :layout)
        action-method
        (fn [request]
          (let [route-params (:route-params request)
                id (get route-params "id")
                update-action (if (fn? update-action)
                                (update-action request)
                                update-action)
                create-action (if (fn? create-action)
                                (create-action request)
                                create-action)]
            [(replace-params route-params
                             (cond (and id update-action) update-action
                                   create-action create-action
                                   :else (:uri request)))
             (cond (and id update-method) update-method
                   create-method create-method
                   :else :post)]))
        layout (or layout (grid-layout))]
    (SandbarForm. form-name action-method layout attributes)))

;; ==========================
;; Form View Request Handling
;; ==========================

(defn get-params
  "Get params from a map where the keys may be strings or keywords."
  [keys params]
  (reduce (fn [a b]
            (let [v (get-param params b)]
              (assoc a b v)))
          {}
          keys))

(defn marshal [params]
  (let [keys (map keyword (keys params))]
    (get-params keys params)))

(defn form-view [fields view-processor request]
  (let [params (marshal (:params request))
        form-info {:form-data nil
                   :params params
                   :errors nil
                   :request request
                   :response {}
                   :fields (if (fn? fields)
                             (fields request)
                             fields)
                   :i18n {}}]
    (view-processor form-info)))

;; =======================
;; High Level Constructors
;; =======================

(defn make-processor [form load defaults]
  (let [processors [(add-errors form)
                    (add-previous-input form)
                    load
                    defaults
                    (add-cancel)
                    (render-form form)]]
    (apply comp (reverse processors))))

(defn embedded-form
  "Create an embedded form handler."
  [form fields & {:keys [before-data
                         load
                         defaults 
                         before-control
                         controls
                         before-render
                         after-render
                         processor]
                  :as options}]
  (let [defaults (if defaults
                   (add-defaults defaults)
                   identity)
        load (if load
               (add-source load)
               identity)
        before-data (or before-data identity)]
    (EmbeddedFormHandler. (partial form-view
                                   fields
                                   (make-processor form load defaults)))))

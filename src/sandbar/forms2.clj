;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Brenton Ashworth"}
  sandbar.forms2
  "Form layout and processing. A set of protocols are defined for working with
  forms. Default implementations of the protocols are provided. Form
  implementors may build forms using a combination of the defaults and custom
  implementations of the protocols. Finally, constructor functions are provided
  to allow for concise form definitions.

  In all dealings with forms, form data is represented as a map of simple data
  types. This may include strings, numbers and vectors of such. For example:

  {:name \"Brenton\"
   :age 10
   :region 2
   :roles [\"admin\" \"user\"]
   :languages [1 2 3]}

  When data is loaded from an external data source, it must be transformed
  into this format. When a form is submitted, the data will arrive in this
  format for validation and all response functions will receive it in this
  way.

  Several protocols below take an env argument. This will be a map which may
  contain the keys:

  :errors A map of field names to error vectors. Each error vector will
          contain one or more strings which are the error messages for that
          field.
  :labels A map of field names to strings. Each string is the label to be used
          for that field. This will allow all labels to be placed into a
          single map and will also be used for internationalization."
  (:require [hiccup.core :as html]
            [clojure.string :as string]
            [sandbar.stateful-session :as session]))

;; See sandbar.forms2.example-basic for usage.

;; ==============
;; Form protocols
;; ==============
;; This is not the complete set of form protocols. I have an idea of
;; what they might be and I will be adding them here as they are implemented.

(defprotocol Field
  "The format of the data and env arguments is described above."
  (field-map [this data env] "Return a map of field data")
  (render-field [this data env] "Return renderd HTML for this field"))

(defprotocol Form
  "Render form HTML."
  (render-form [this request data env] "Return rendered HTML for this form"))

(defprotocol Layout
  "Layout form fields and render as HTML. The format of the data and env
  arguments is described above."
  (render-layout [this request fields buttons data env]
                 "Return rendered HTML for all form fields"))

(defprotocol Button
  "Render a form button."
  (render-button [this] "Return rendered HTML for this button"))

(defprotocol Defaults
  "Get the default data for a form."
  (default-form-data [this request] "Return map of default form data"))

(defprotocol DataSource
  "Load data for a form from an external data source."
  (load-form-data [this request] "Return map of form data"))

(defprotocol TempStorage
  "Store information that will be needed for multiple requests."
  (temp-put! [this key data])
  (temp-get [this key]))
  
(defprotocol Control
  "Add hidden control fields during the get request and then determine if the
  form was cancel or double posted when the form is submitted. May also store
  control information in the session."
  (add-control [this request fields] "Return a list of FormFields.")
  (status [this request] "Return a Ring response or nil."))
  
(defprotocol FormView
  "Process a complete form view request. Return a map with form information
  to allow the form to be embedded in a page."
  (view-form [this request] "Return a map of :title :body :errors."))

;; =======================
;; Default implementations
;; =======================

;;
;; Form fields
;;

(defn field-type-dispatch
  [field-map]
  (:type field-map))

(defmulti field-label
  "Create a field label for a form element."
  field-type-dispatch)

(defmethod field-label :default
  [{:keys [label required]}]
  (let [div [:div {:class "field-label"} label]]
    (if required
      (vec (conj div [:span {:class "required"} "*"]))
      div)))

(defmulti form-field-cell
  "Create a field cell which includes the field, field label and optionally
  on error message."
  field-type-dispatch)

(defmethod form-field-cell :default
  [{:keys [id html errors] :as field-map}]
  (let [error-message (first errors) ;; TODO show all errors
        error-attrs (if id {:id (str (name id) "-error")} {})
        error-attrs (if error-message
                      error-attrs
                      (merge error-attrs {:style "display:none;"}))]
    [:div.sandbar-field-cell
     (field-label field-map)
     [:div.sandbar-error-message error-attrs error-message]
     html]))

(defrecord Textfield [field-name attributes] Field
  (field-map [this data env]
             {:type :textfield
              :field-name field-name
              :label (or (:label this) (get (:labels env) field-name))
              :value (or (field-name data) "")
              :errors (get (:errors env) field-name)
              :required (:required this)
              :id (:id attributes)})
  (render-field [this data env]
                (let [d (field-map this data env)
                      field-html [:input
                                  (merge {:type (:type d)
                                          :name (name field-name)
                                          :value (:value d)
                                          :class "textfield"}
                                         attributes)]]
                  (html/html (form-field-cell (assoc d :html field-html))))))

(defrecord SandbarButton [type label] Button
  (render-button [this]
                 (html/html [:input.sandbar-button
                             {:type "submit"
                              :value label
                              :name (name type)}])))

(defn button [type & {:keys [label] :as options}]
  (let [label (or label (case type
                              :save "Save"
                              :cancel "Cancel"
                              "Submit"))]
    (SandbarButton. type label)))

;;
;; Form layout
;;

(defrecord GridLayout [] Layout
  (render-layout [this request fields buttons data env]
    (let [rendered-fields (map #(render-field % data env) fields)
          rendered-buttons (map #(render-button %) buttons)]
      (html/html [:table
                  [:tr
                   [:td
                    [:div rendered-fields]
                    [:div.buttons
                     [:span.basic-buttons rendered-buttons]]]]]))))

;;
;; Form
;;

(defrecord SandbarForm [fields buttons action-method layout attributes] Form
  (render-form [this request data env]
    (let [fields (if (fn? fields)
                   (fields request)
                   fields)
          [action method] (action-method request)
          body (render-layout layout request fields buttons data env)
          method-str (.toUpperCase (name method))]
      (html/html
       [:div.sandbar-form
        (-> (if (contains? #{:get :post} method)
              [:form (merge {:method method-str :action action} attributes)]
              [:form (merge {:method "POST" :action action} attributes)
               [:input :type "hidden" :name "_method" :value method-str]])
            (conj body)
            (vec))]))))

;;
;; Data
;;

(extend-protocol Defaults
  clojure.lang.IFn
  (default-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (default-form-data [this request] this))

(extend-protocol DataSource
  clojure.lang.IFn
  (default-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (default-form-data [this request] this))

(defrecord FlashStorage [] TempStorage
  (temp-put! [this key data] (session/flash-put! key data))
  (temp-get [this key] (session/flash-get key)))

;; ============
;; Constructors
;; ============

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

(defn grid-layout
  "This will implement all of the features of the current grid layout."
  []
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
  [fields & {:keys [create-method update-method create-action
                    update-action layout buttons]
             :as options}]
  (let [attributes (dissoc options
                           :create-method :update-method :create-action
                           :update-action :layout :buttons)
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
        layout (or layout (grid-layout))
        buttons (or buttons [(button :submit) (button :cancel)])]
    (SandbarForm. fields buttons action-method layout attributes)))

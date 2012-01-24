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
  way."
  (:use [clojure.tools.macro :only [name-with-attributes]]
        [ring.util.response :only [redirect]]
        ;; This will be removed
        [compojure.core :only [routes GET POST PUT]]
        [sandbar.core :only [get-param]]
        [sandbar.validation :only [validation-errors]])
  (:require [hiccup.core :as html]
            [clojure.string :as string]))

;; Form Protocols
;; ==============

(defprotocol Html
  (render [this form-info] "Return rendered HTML."))

(defprotocol Field
  (field-map [this] [this data]
             "Return a map of the field's :type, :name and :value."))

(defprotocol Resource
  (new-uri [this])
  (create-uri [this])
  (edit-uri [this])
  (update-uri [this])
  (submit-method [this request])
  (submit-action [this request])
  (resource-id [this request])
  (create-routes [this view-handler submit-handler]))

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

;; Form Fields
;; ===========

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

(defn textfield
  "Create a form textfield. The first argument is the field name. Optional
  named arguments are label and required. Any other named arguments will be
  added to the field's html attributes.

  Examples:

    (textfield :age)
    (textfield :age :label \"Age\")
    (textfield :age :label \"Age\" :required true)
    (textfield :age :label \"Age\" :required true :size 50)
    (textfield :age :size 50 :id :age :value \"x\")

  "
  [field-name & {:keys [label required] :as options}]
  (let [attributes (-> (merge {:size 35} options)
                       (dissoc :label :required))
        field (Textfield. field-name attributes)
        field (if label (assoc field :label label) field)
        field (if required (assoc field :required required) field)]
    field))

(defn password
  "Same as textfield but sets the :type attribute to \"password\".

   Same as calling:
     (textfield :password :label <label> :type \"password\").

  "
  [field-name & {:keys [] :as options}]
  (apply textfield field-name (flatten (seq (assoc options :type "password")))))

(defn button [type & {:keys [label] :as options}]
  (let [label (or label (case type
                              :save "Save"
                              :cancel "Cancel"
                              "Submit"))]
    (Button. type label)))

(defn hidden [type & {:keys [value] :as options}]
  (if value
    (Hidden. type value)
    (Hidden. type "")))

;; Form layout
;; ===========

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

(defn grid-layout
  "This will implement all of the features of the current grid layout."
  [& {:keys [] :as options}]
  (GridLayout.))

;; Utilities
;; =========

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

(defn replace-params
  "Replace all routes params by values contained in the given params map."
  [m s]
  (reduce (fn [uri entry]
            (let [k (first entry)]
              (string/replace-first uri
                                    (if (keyword? k)
                                      (str k)
                                      (str ":" k))
                                    (second entry))))
          s
          m))

;; Form View
;; =========

(defrecord RestfulResource [root id]
  Resource
  (new-uri [this] (str root "/new"))
  (create-uri [this] (str root))
  (edit-uri [this] (str root "/" id "/edit"))
  (update-uri [this] (str root "/" id))
  (resource-id [this request]
    (let [route-params (:route-params request)]
      (get-param route-params id)))
  (submit-method [this request]
    (let [id (resource-id this request)]
      (if id :put :post)))
  (submit-action [this request]
    (let [route-params (:route-params request)
          id (resource-id this request)
          update-action (update-uri this)
          create-action (create-uri this)]
      (replace-params route-params
                      (cond (and id update-action) update-action
                            create-action create-action
                            :else (:uri request)))))
  (create-routes [this view-handler submit-handler]
    (routes
     ;; TODO: Implement this is pure Ring. No need to depend on Compojure.
     (GET (new-uri this) request (process-request view-handler request))
     (POST (create-uri this) request (process-request submit-handler request))
     (GET (edit-uri this) request (process-request view-handler request))
     (PUT (update-uri this) request (process-request submit-handler request)))))

(defn restful-resource [root id]
  (RestfulResource. root id))

(defrecord SandbarForm [form-name resource layout attributes]
  Form
  (unique-id [_] form-name)
  Html
  (render [_ {:keys [request] :as form-info}]
    (let [action (submit-action resource request)
          method (submit-method resource request)
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

(defn form
  "Create a form..."
  [form-name & {:keys [resource layout] :as options}]
  (let [attributes (dissoc options :resource :layout)
        layout (or layout (grid-layout))]
    (SandbarForm. form-name resource layout attributes)))

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

(defrecord EmbeddedFormHandler [fields processor]
  FormHandler
  (process-request [_ request]
                   (form-view fields processor request)))

(defrecord FormPageHandler [layout embedded-form]
  FormHandler
  (process-request [_ request]
                   (layout request
                           (:response
                            (process-request embedded-form request)))))

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

(defn add-source [resource load-fn]
  (fn [form-info]
    (let [form-data (:form-data form-info)
          id (resource-id resource (:request form-info))]
      (cond form-data form-info
            id (assoc form-info :form-data (load-fn id))
            :else form-info))))

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

;; Form Processing
;; ===============

(defrecord RedirectResponse [form cancel-uri success-fn] SubmitResponse
  (canceled [this form-info] (redirect cancel-uri))
  (failure [this {:keys [form-data errors]}]
           (fn [request]
             (let [failure-uri (get (-> request :headers) "referer")]
               {:status 302
                :headers {"Location" failure-uri}
                :flash {(unique-id form) {:errors errors :data form-data}}
                :body ""})))
  (success [this form-info] (redirect (success-fn (:form-data form-info)))))

(defn redirect-response [form cancel-uri success-fn]
  (RedirectResponse. form cancel-uri success-fn))

(defrecord FunctionValidate [vfn] SubmitProcessor
  (process-submit [this respond {:keys [form-data] :as form-info}]
    (let [errors (validation-errors (vfn form-data))]
      (if errors
        (let [form-info (assoc form-info :errors errors)]
          (assoc form-info :response (failure respond form-info)))
        form-info))))

(defn validator-function [vfn]
  (FunctionValidate. vfn))

(defrecord CancelControl [] SubmitProcessor
  (process-submit [this respond {:keys [request form-data] :as form-info}]
    (let [cancel-name (keyword (:_cancel form-data))
          cancel (cancel-name form-data)
          form-data (dissoc form-data :_cancel cancel-name)
          form-info (assoc form-info :form-data form-data)]
      (if cancel
        (assoc form-info :response (canceled respond form-info))
        form-info))))

(defn cancel-control []
  (CancelControl.))

(defrecord FormDataCleaner [] SubmitProcessor
  (process-submit [this respond {:keys [request form-data] :as form-info}]
    (let [form-data (dissoc form-data :_method :save :cancel)]
      (assoc form-info :form-data form-data))))

(defn form-data-cleaner []
  (FormDataCleaner.))

(defn process-form-submit
  [respond processors form-info]
  (loop [processors processors
         form-info form-info]
    (if (or (:response form-info) (not (seq processors)))
      form-info
      (recur (rest processors) (process-submit (first processors)
                                               respond
                                               form-info)))))

(defrecord SubmitHandler [fields respond controls validator] FormHandler
  (process-request [this request]
    (let [{:keys [response] :as form-info}
          (process-form-submit respond
                               (conj controls (form-data-cleaner) validator)
                               {:request request
                                :form-data (-> request :params marshal)
                                :fields fields})]
      (or response (success respond form-info)))))

;; Builders
;; ============

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
  [form resource fields & {:keys [before-data
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
               (add-source resource load)
               identity)
        before-data (or before-data identity)]
    (EmbeddedFormHandler. fields (make-processor form load defaults))))

(defn form-page
  [form resource fields layout & options]
  (FormPageHandler. layout (apply embedded-form form resource fields options)))

(defn submit-handler
  [fields respond & {:keys [controls validator] :as options}]
  (let [controls (or controls [(cancel-control)])
        validator (cond (nil? validator) (validator-function identity)
                        (satisfies? SubmitProcessor validator) validator
                        :else (validator-function validator))]
    (SubmitHandler. fields respond controls validator)))

;; Combined Builders
;; =================

(defn make-form
  [name & {:keys [on-cancel on-success validator resource fields layout
                  page-layout load]
           :as options}]
  (let [resource resource
        fields fields
        validator validator
        layout (or layout (grid-layout))
        page-layout (or page-layout (fn [_ body] body))
        form (form name
                   :resource resource
                   :layout layout)
        view-handler (form-page form
                                resource
                                fields
                                page-layout
                                :load load)
        responder
        (redirect-response form
                           on-cancel
                           on-success)
        submit-handler (submit-handler fields
                                       responder
                                       :validator validator)]
    (create-routes resource view-handler submit-handler)))

(defmacro defform [name & options]
  "Define a form handler function. The name may optionally be
  followed by a doc-string and metadata map. See make-form for details."
  (let [[name options] (name-with-attributes name options)]
    `(def ~name (make-form ~(keyword name) ~@options))))

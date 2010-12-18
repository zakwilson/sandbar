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
          single map and will make internationalization simple to implement."
  (:use [sandbar.core :only [get-param]])
  (:require [hiccup.core :as html]
            [clojure.string :as string]
            [sandbar.stateful-session :as session]))

;; See sandbar.forms2.example-basic for usage.

;; ==============
;; Form protocols
;; ==============

(defprotocol Field
  "The format of the data and env arguments is described above. A field type is
  a keyword. Standard types include: :textfield :textarea :cancel-button
  :submit-button :hidden :checkbox, etc."
  (field-map [this] [this data]
             "Return a map of the field's :type, :name and :value.")
  (render-field [this data env] "Return renderd HTML for this field"))

(defprotocol Layout
  "Layout form fields and render as HTML. The format of the data and env
  arguments is described above. This function returns a map with a key :body
  where the value is the rendered field HTML. Depending on the implementation
  optional keys may be added to pass other information back from the layout."
  (render-layout [this request fields data env]
                 "Return a map containing :body and other optional keys"))

(defprotocol Form
  "Render form HTML."
  (unique-id [this] "Return a unique identifier for this form")
  (render-form [this request fields data env]
               "Return a map containing :body and other optional keys"))

(defprotocol Defaults
  "Get the default data for a form."
  (default-form-data [this request] "Return map of default form data"))

(defprotocol DataSource
  "Load data for a form from an external data source."
  (load-form-data [this request] "Return map of form data"))

(defprotocol TempStorage
  "Store information that will be needed for multiple requests."
  (get-temp [this form key request] "Returns data for this form and key")
  (put-temp! [this form key data]
             "Returns a map of data which can be merged into the Ring response.
              May use side effects to store data."))
  
(defprotocol Control
  "Add hidden control fields during the GET request and then determine if the
  form was canceled or double posted when the form is submitted. May also store
  control information in temp storage."
  (add-control [this request fields] "Return a list of FormFields.")
  (status [this request response] "Return a Ring response or nil."))
  
(defprotocol FormHandler
  "Process a complete form view request. There may be many different kinds
  of form handlers. Depending on the implementation, my return a Ring response
  map or map to be used as an intermediate result."
  (process-request [this request]
                   "Return a map containing :body and other optional keys."))

(defprotocol Validate
  "The map returned from validate will contain one key for each field. For each
  key the value will be a list of error messages."
  (validate [this data] "Return a map of field names to validation errors."))

(defprotocol Response
  "Collaborators: TempStore."
  (canceled [this data] "Return a Ring response map.")
  (failure [this data errors] "Return a Ring response map.")
  (success [this data] "Return a Ring response map."))

(defprotocol Routes
  "Collaborators: FormHandler"
  (routes [this] "Returns a routing function."))

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

(defrecord Textfield [field-name attributes] Field
  (field-map [this] (field-map this {}))
  (field-map [this data] {:type :textfield
                          :name field-name
                          :value (or (field-name data) "")})
  (render-field [this data env]
    (let [d (merge (field-map this data)
                   {:label (or (:label this)
                               (get (:labels env) field-name))
                    :errors (get (:errors env) field-name)
                    :required (:required this)
                    :id (:id attributes)})
         field-html [:input
                     (merge {:type "text"
                             :name (name field-name)
                             :value (:value d)
                             :class "textfield"}
                            attributes)]]
     (html/html (form-field-cell (assoc d :html field-html))))))

(defrecord Hidden [field-name value] Field
  (field-map [this] (field-map this {}))
  (field-map [this data] {:type :hidden
                          :name field-name
                          :value (or (get data field-name)
                                     value
                                     "")})
  (render-field [this data env]
                [:input {:type "hidden"
                         :name (name field-name)
                         :value (:value (field-map this data))}]))

(defrecord Button [field-name value] Field
  (field-map [this] {:type (if (= field-name :cancel)
                             :cancel-button
                             :submit-button)
                     :name field-name
                     :value value})
  (field-map [this data] (field-map this))
  (render-field [this data env]
                (html/html [:input.sandbar-button
                            {:type "submit"
                             :name (name field-name)
                             :value value}])))

;;
;; Form layout
;;

(defn button? [field]
  (contains? #{:cancel-button :submit-button} (:type (field-map field))))

(defrecord GridLayout [title] Layout
  (render-layout [this request fields data env]
    (let [buttons (filter button? fields)
          fields (filter (complement button?) fields)
          rendered-fields (map #(render-field % data env) fields)
          rendered-buttons (map #(render-field % data env) buttons)
          title (if (fn? title)
                  (title request)
                  title)
          body (html/html [:table
                           [:tr
                            [:td
                             [:div rendered-fields]
                             [:div.buttons
                              [:span.basic-buttons rendered-buttons]]]]])
          result {:body body}]
      (if title
        (assoc result :title title)
        result))))

;;
;; Form
;;

(defrecord SandbarForm [form-name action-method layout attributes] Form
  (unique-id [this] form-name)
  (render-form [this request fields data env]
    (let [[action method] (action-method request)
          layout (render-layout layout request fields data env)
          method-str (.toUpperCase (name method))]
      (assoc layout
        :body
        (html/html
         [:div.sandbar-form
          (-> (if (contains? #{:get :post} method)
                [:form (merge {:method method-str :action action} attributes)]
                [:form (merge {:method "POST" :action action} attributes)
                 [:input {:type "hidden" :name "_method" :value method-str}]])
              (conj (:body layout))
              (vec))])))))

;;
;; Data
;;

(extend-protocol Defaults
  clojure.lang.Fn
  (default-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (default-form-data [this request] this))

(extend-protocol DataSource
  nil
  (load-form-data [this request] nil)
  clojure.lang.Fn
  (load-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (load-form-data [this request] this))

(extend-protocol TempStorage
  nil
  (put-temp! [this form key data] nil)
  (get-temp [this form key request] nil)
  clojure.lang.IPersistentMap
  (put-temp! [this form k data] (merge {:flash {(unique-id form) {k data}}}
                                         this))
  (get-temp [this form k request] (get this k)))

(defrecord FlashStorage [] TempStorage
  (put-temp! [this form key data] (session/flash-put! key data))
  (get-temp [this form key request] (session/flash-get key)))

;;
;; Form Handling
;;

(defrecord EmbeddedFormHandler [form fields controls temp-store data-source
                                defaults]
  FormHandler
  (process-request [this request]
    (let [errors (get-temp temp-store form :errors request)
          data (when errors
                 (get-temp temp-store form :data request))
          data (or data
                   (load-form-data data-source request)
                   (default-form-data defaults request))
          env (if errors {:errors errors} {})
          fields (if (fn? fields)
                   (fields request)
                   fields)
          fields (if (seq controls)
                   (reduce (fn [f next-control]
                             (add-control next-control request f))
                           fields
                           controls)
                   fields)
          result (render-form form request fields data env)]
      (if errors
        (assoc result :errors errors)
        result))))

(defn marshal [params]
  (let [keys (map keyword (keys params))
        params (zipmap keys (vals params))]
    params))

(defrecord SubmitHandler [response controls] FormHandler
  (process-request [this request]
    (if-let [result (reduce (fn [s next-control]
                              (or s (status next-control request response)))
                            nil
                            controls)]
      result
      (let [params (:params request)
            data (marshal params)]
        (success response data)))))

;;
;; Control
;;

(defrecord CancelControl [] Control
  (add-control [this request fields]
    (if-let [cancel-buttons (->> fields
                                 (filter button?)
                                 (map field-map)
                                 (filter #(= (:type %) :cancel-button)))]
      (let [h (vec (map #(Hidden. :_cancel (:name %))
                        cancel-buttons))]
        (vec (concat fields h)))
      fields))
  (status [this request response]
          (let [params (:params request)
                cancel-name (keyword (get-param params :_cancel))
                data (dissoc (marshal params) :_cancel)]
            (when (get-param params cancel-name)
              (let [data (dissoc data cancel-name)]
                (canceled response data))))))

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

(defn button [type & {:keys [label] :as options}]
  (let [label (or label (case type
                              :save "Save"
                              :cancel "Cancel"
                              "Submit"))]
    (Button. type label)))

(defn grid-layout
  "This will implement all of the features of the current grid layout."
  [& {:keys [title] :as options}]
  (let [title (or title "")]
    (GridLayout. title)))

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

(defn cancel-control []
  (CancelControl.))

(defn embedded-form
  "Create an embedded form handler."
  [form fields & {:keys [temp-store data-source defaults controls]
                  :as options}]
  (let [controls (or controls [(cancel-control)])
        defaults (or defaults {})]
    (EmbeddedFormHandler. form
                          fields
                          controls
                          temp-store
                          data-source
                          defaults)))

(defn submit-handler
  [response & {:keys [controls] :as options}]
  (let [controls (or controls [(cancel-control)])]
    (SubmitHandler. response controls)))

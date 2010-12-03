;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.forms
  "Forms and form layouts."
  (:use [ring.util.response :only [redirect]]
        [compojure.core :only [routes GET POST]]
        [sandbar.stateful-session :only [flash-put!
                                         flash-get]]
        [sandbar.core :only [cpath get-param property-lookup]]
        [sandbar.validation :only [if-valid
                                   required-fields
                                   build-validator
                                   validation-errors]])
  (:require [compojure.route :as route]
            [clojure.string :as string]
            [clojure.contrib.json :as json]))

;;
;; Utilities
;;

(defn store-errors-and-redirect [name redirect-page]
  (fn [form-data errors]
    (do (flash-put! name
                    (merge {:form-data form-data} errors))
        redirect-page)))

(defn get-params
  "Get params from a map where the keys may be strings or keywords."
  [keys params]
  (reduce (fn [a b]
            (let [v (get-param params b)]
              (assoc a b v)))
          {}
          keys))

(defn filter-nil-vec [coll]
  (vec (filter #(not (nil? %)) coll)))

(defn form-cancelled?
  ([params]
     (form-cancelled? params "Cancel"))
  ([params value]
     (let [cancel-values (conj #{"cancel" "Cancel"} value)
           cancel (get-param params :cancel)]
       (contains? cancel-values cancel))))

(defn nil-or-empty-string? [v]
  (or (not v)
      (and (string? v)
           (empty? v))))

(defn clean-form-input
  "Set empty values to nil and remove the id if it is nil."
  [m]
  (let [original-meta (meta m)]
    (with-meta
      (apply merge
             (map #(hash-map (first %)
                             (let [value (last %)]
                               (if (nil-or-empty-string? value)
                                 nil
                                 value)))
                  (if (not (nil-or-empty-string? (:id m)))
                    m
                    (dissoc m :id))))
      original-meta)))

;;
;; Form Elements
;;

(defn field-label [title key req]
  (let [title (if (map? title)
                (property-lookup title key)
                title)
        req (cond (keyword? req) req
                  (contains? (set req) key) :required
                  :else :optional)]
    [:div {:class "field-label"} title
     (if (= req :required) [:span {:class "required"} "*"] "")]))

(defn hidden [fname]
  {:type :hidden
   :label ""
   :field-name fname
   :html [:input {:type "hidden" :name (name fname) :value ""}]})

#_(defn textarea
  ([title fname] (textarea title fname {} :optional))
  ([title fname options] (textarea title fname options :optional))
  ([title fname options req]
     {:type :textarea
      :label (field-label title fname req)
      :field-name fname
      :html [:textarea (merge {:name (name fname)} options)]}))

#_(defn htmlfield
  ([fname content]
     (htmlfield nil content {:id fname}))
  ([title content options & more]
     (let [fname (:id options)]
       {:type :htmlfield
        :label (if title (field-label title fname :optional) [:div])
        :field-name fname
        :html [:div options content]})))

(defn textfield
  "Create a form textfield. The first argument is the field name. Optional
  named arguments are title and required. Any other named arguments will be
  added to the field's html attributes.

  Examples:

  (textfield :age)
  (textfield :age :title \"Age\")
  (textfield :age :title \"Age\" :required true)
  (textfield :age :title \"Age\" :required true :size 50)
  (textfield :age :size 50 :id :age :value \"x\")"
  [field-name & {:keys [title required] :as options}]
  (let [options (-> (merge {:size 35} options)
                    (dissoc :title :required))]
    (assoc {:type :textfield
            :label (fn [t r]
                     (field-label (or title t) field-name r))
            :field-name field-name
            :html [:input
                   (merge {:type "Text" :name (name field-name) :value ""
                           :class "textfield"} options)]}
      :required (true? required))))

(defn password
  "Create a form password field. See doc for textfield."
  [& args]
  (let [textfield (apply textfield args)]
    (-> textfield
        (assoc :type :password)
        (assoc :html [:input (merge (last (:html textfield))
                                    {:type "Password"})]))))

(defn checkbox
  "Create a form checkbox. The first argument is the field name. The named
  argument title is optional. Any other named arguments will be added to the
  field's html attributes.

  Examples:

  (checkbox :elated)
  (checkbox :elated :title \"Elated\")
  (checkbox :elated :title \"Elated\" :id :elated)"
  [field-name & {:keys [title] :as options}]
  {:type :checkbox
   :label (fn [t]
            [:span {:class "field-label"} (or title
                                              (if (map? t)
                                                (get t field-name field-name)
                                                t))])
   :field-name field-name
   :html [:input
          (merge {:type "checkbox"
                  :name (name field-name)
                  :value "checkbox-true"} options)]
   :required false})

(defn wrap-checkboxes-in-group [coll]
  [:div {:class "group"}
     (map #(vector :div {:class "group-checkbox"} %) coll)])

#_(defn multi-checkbox
  ([props many-spec]
     (multi-checkbox props
                     (:alias many-spec)
                     ((:all-items many-spec))
                     (:name-fn many-spec)))
  ([props fname coll value-fn]
     {:type :multi-checkbox
      :label [:span {:class "group-title"} (property-lookup props fname)]
      :field-name fname
      :html (wrap-checkboxes-in-group
              (map
               #(let [value (value-fn %)]
                  [:input
                   {:type "checkbox" :name fname :value value}
                   (property-lookup props (keyword value))])
               coll))
      :value-fn value-fn}))

#_(defn- options [coll key-key value-key]
  (map #(vector :option {:value  (key-key %)} (value-key %)) coll))

#_(defn select
  "Create a select form element."
  ([title fname coll kvs & optional]
     (let [key-and-value (first (dissoc kvs :prompt))
           prompt (first (:prompt kvs))
           k (key key-and-value)
           v (val key-and-value)
           opts (first (filter map? optional))
           req (first (filter #(not (map? %)) optional))
           options (options coll k v)
           select-html [:select (merge {:name (name fname)} opts)]
           select-html (if prompt
                         (concat select-html
                                 [[:option
                                   {:value (key prompt)} (val prompt)]])
                         select-html)]
       {:type :select
        :label (field-label title fname req)
        :field-name fname
        :html (vec
               (concat
                select-html
                options))
        :value-fn k})))

#_(defn multi-select [title fname coll kv & optional]
  (let [kv (dissoc kv :prompt)
        opts (merge {:multiple true :size 5} (first (filter map? optional)))
        req (first (filter #(not (map? %)) optional))]
    (select title fname coll kv opts req)))

(defn form-to
  [[method action attrs] & body]
  (let [method-str (.toUpperCase (name method))]
    (-> (if (contains? #{:get :post} method)
          [:form (merge {:method method-str, :action action} attrs)]
          [:form (merge {:method "POST", :action action} attrs)
           (hidden (str "_method") method-str)])
        (concat body)
        (vec))))

;;
;; Buttons
;;

(defn cancel-button
  ([] (cancel-button "Cancel"))
  ([v]
     [:input {:type "submit" :value v :name "cancel"
              :class "sandbar-button"}]))

(defn submit-button
  ([] (submit-button "Submit"))
  ([v]
     [:input {:type "submit" :value v :name "submit"
              :class "sandbar-button"}]))

(defn reset-button
  ([] (reset-button "Reset"))
  ([v]
     [:input {:type "reset" :value v :class "sandbar-button"}]))

(defn create-form-button [[k visible-name]]
  (case k
        :submit (submit-button visible-name)
        :save (submit-button visible-name)
        :cancel (cancel-button visible-name)
        :reset (reset-button visible-name)
        :save-and-new (submit-button visible-name)
        :submit-and-new (submit-button visible-name)
        (submit-button visible-name)))

(defn compile-buttons [buttons]
  (let [labels (take 2 (reverse buttons))
        labels (if (= (second labels) :labels) (first labels) nil)
        buttons (take-while #(not (keyword? %)) buttons)]
    (map #(cond (= (count %) 1)
                (let [button-type (first %)
                      value (button-type labels)
                      value (or value (string/capitalize
                                       (name button-type)))]
                  [button-type value])
                (and (= (count %) 2) (keyword? (second %)))
                [(first %) (get labels (second %) (second %))]
                :else %)
         buttons)))

(defn special-button-text
  "Returns a map which contains the value of the Cancel and Save and New
   buttons. These values may be set by the user, so they can be anything, and
   we need them in order to decide were to redirect after a form submission."
  [buttons]
  (let [buttons (compile-buttons buttons)
        cancel (second (first (filter #(= (first %) :cancel) buttons)))
        save-and-new (second
                      (first
                       (filter #(contains? #{:save-and-new :submit-and-new}
                                           (first %))
                               buttons)))]
    {:cancel cancel :save-and-new save-and-new}))

(defn display-buttons [class-prefix buttons]
  (let [buttons (compile-buttons buttons)]
    (vec
     (concat
      [:span {:class (str class-prefix "-buttons")}]
      (interleave (map create-form-button buttons) (cycle ["&nbsp;"]))))))

(defn- get-colspan [rows]
  (let [cells (drop 1 (first rows))]
    (reduce (fn [colspan cell]
              (if (map? (second cell))
                (+ colspan (Integer/valueOf (or (:colspan (second cell)) 1)))
                (+ colspan 1)))
            0
            cells)))

(defn append-buttons-to-table
  "Append a row of buttons to the end of the table within the passed div. The
   table can be in any location within the div but there can be only one."
  [div buttons]
  (let [[beginning end] (split-with #(not (and (coll? %)
                                               (= :table (first %)))) div)
        table (first end)
        end (drop 1 end)
        colspan (get-colspan (rest table))
        new-table (when table
                    (conj table
                          [:tr 
                           [:td {:colspan colspan}
                            [:div.buttons
                             (display-buttons "basic" buttons)]]]))]
    (vec (if new-table
           (concat beginning [new-table] end)
           div))))

;;
;; Marshal and Binding data
;;

(defn get-yes-no-fields
  "Get Y or N values for all keys in cb-set. These keys represent checkboxes
   which must have either Y or N value. If the checkbox is not present then
   is was not selected and is a N."
  [m params cb-set]
  (let [new-map (reduce
                 (fn [a b]
                   (let [k (key b)
                         k (if (keyword? k) k (keyword k))]
                     (if (and (contains? cb-set k)
                              (= "checkbox-true" (val b)))
                       (assoc a k "Y")
                       a)))
                 m
                 params)]
    (reduce (fn [a b] (if (b a) a (assoc a b "N")))
            new-map
            cb-set)))

(defn get-multi-checkbox
  "Add the key k to the map m where the value of k is is a vector of
   selected values."
  [m params k all-values name-fn]
  (let [v (get-param params k)
        selected-values (set (filter-nil-vec (if (or (number? v)
                                                     (string? v)) [v] v)))
        selected (filter #(contains? selected-values (name-fn %)) all-values)]
    (assoc m k selected)))

(defn get-multi-select
  "Add the key k to the map m where the value of k is a vector of
   selected values."
  [m params k all-values name-fn & more]
  (let [name-fn (if (map? name-fn) (key (first name-fn)) name-fn)]
    (get-multi-checkbox m params k all-values name-fn)))

(defn- set-form-field-value* [form-state input-field update-fn]
  (let [field-name (:field-name input-field)
        previous-val ((keyword field-name) (:form-data form-state))]
    (if previous-val
      (update-fn previous-val (:html input-field))
      input-field)))

(defn set-input-form-field-value [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (assoc input-field :html
            (vector :input
                    (assoc
                        (last html) :value previous-value))))))

(defmulti set-form-field-value (fn [a b] (:type b)))

(defmethod set-form-field-value :default [state field]
           field)

(defmethod set-form-field-value :textfield [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :hidden [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :password [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :checkbox [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (assoc input-field :html
            (vector :input (if (= previous-value "Y")
                             (assoc (last html) :checked "true")
                             (last html)))))))

(defmethod set-form-field-value :textarea [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (assoc input-field :html (conj html previous-value)))))

(defmethod set-form-field-value :select [form-state input-field]
  (let [field-name (:field-name input-field)
        input-html (:html input-field)
        previous-val ((keyword field-name) (:form-data form-state))
        values (if (coll? previous-val)
                 (map #((:value-fn input-field) %) previous-val)
                 [previous-val])]
    (if (seq values)
      (assoc input-field :html
             (apply vector
                    (map #(if (and (vector? %)
                                   (= :option (first %))
                                   (some (fn [x]
                                           (= (:value (second %)) x))
                                         values))
                            [:option {:value (:value (second %))
                                      :selected "selected"}
                             (last %)]
                            %)
                         input-html)))
      input-field)))

(defmethod set-form-field-value :multi-checkbox [form-state input-field]
  (let [title (:label input-field)
        checkboxes (map last (last (:html input-field)))
        field-name (:field-name input-field)
        field-value (field-name (:form-data form-state))
        value-set (set (map #((:value-fn input-field) %) field-value))
        new-checkboxes (map #(vector :input
                                     (let [attrs (second %)]
                                       (if (contains? value-set
                                                     (:value attrs))
                                        (assoc attrs :checked "true")
                                        attrs))
                                     (last %))
                            checkboxes)]
    (assoc input-field :html
           (wrap-checkboxes-in-group new-checkboxes))))

;;
;; Layout
;;

(defn layout-table
  "Create a table using the layout vector. The layout vector is a vector of
   integers, one for each row indicating the number of cell that go in that
   row. If the number of cells in a row is less than the max value then
   the last cell will be given a colspan value that is large enough to fill
   the table."
  [layout & cell-values]
  (loop [layout (take (count cell-values) (concat layout (repeat 1)))
         max-width (apply max layout)
         cell-values (filter #(not (nil? %)) cell-values)
         rows []]
    (if (seq cell-values)
      (let [row-cell-values (take (first layout) cell-values)
            final-colspan (+ 1 (- max-width (count row-cell-values)))
            row-values (map #(vector 1 %) row-cell-values)
            row-values (if (> final-colspan 1)
                         (concat (butlast row-values)
                                 [[final-colspan (last (last row-values))]])
                         row-values)]
        (recur (rest layout)
               max-width
               (drop (first layout) cell-values)
               (if (seq row-values)
                 (conj rows
                       (vec
                        (concat [:tr]
                                (vec
                                 (map #(let [cell-value (last %)
                                             cell-value (if (vector? cell-value)
                                                          cell-value
                                                          [cell-value])]
                                         (vec
                                          (if (> (first %) 1)
                                            (concat [:td {:valign "top"
                                                          :colspan (first %)}]
                                                    cell-value)
                                            (concat [:td {:valign "top"}]
                                                    cell-value))))
                                      row-values)))))
                 rows)))
      (vec (concat [:table] rows)))))

(defmulti create-form-field-cell (fn [state m p] (:type m)))

(defmethod create-form-field-cell :checkbox [form-state m props]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec [:div
                                   (:html (set-form-field-value form-state m))
                                   (label props)])]
    (if error-message
      [:div
       [:div.error-message error-message]
       field-row]
      field-row)))

#_(defmethod create-form-field-cell :multi-checkbox [form-state m props]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))]
    (if error-message
      [:div
       [:div.error-message error-message]
       label field-row]
      [:div label field-row])))

(defmethod create-form-field-cell :default [form-state m props]
  (let [{:keys [_ label field-name html required]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))
        id (:id (last (:html m)))
        error-id (when id {:id (str (name id) "-error")})]
    [:div.sandbar-field
      (label props (if required :required :optional))
      (if error-message
        [:div.error-message (if id error-id {}) error-message]
        [:div.error-message (merge {:style "display:none;"} error-id)])
      field-row]))

(defmulti create-hidden-field (fn [state m] (:type m)))

(defmethod create-hidden-field :hidden [form-state m]
           (:html (set-form-field-value form-state m)))

(defmethod create-hidden-field :checkbox [form-state m]
           [:input {:type "hidden"
                    :name "__checkboxes"
                    :value (:field-name m)}])

(def one-column-layout (repeat 1))

(defmulti display-form-errors (fn [& args] (first args)))

(defmethod display-form-errors :default [form-name form-errors]
           [:div.form-error-message
            (map #(vector :div %) form-errors)])

(defn form-layout-grid* [form-name layout form-state coll props]
  (let [div (if-let [form-errors (:form form-state)]
              [:div
               (display-form-errors form-name form-errors)]
              [:div])
        the-form
        (conj div 
              (apply layout-table
                     layout
                     (map #(if-let [cell (create-form-field-cell form-state
                                                                 %
                                                                 props)]
                             (vector cell)
                             nil)
                          (filter #(not (= (:type %) :hidden)) coll))))]
    (vec (concat the-form
                 (map #(create-hidden-field form-state %)
                      (filter #(or (= (:type %) :hidden)
                                   (= (:type %) :checkbox)) coll))))))

(defn form-layout-grid
  ([form-name coll request]
     (form-layout-grid one-column-layout form-name coll request {} {}))
  ([layout form-name coll request]
     (form-layout-grid layout form-name coll request {} {}))
  ([layout form-name coll request init-data props]
     (if-let [form-state (flash-get form-name)]
       (form-layout-grid* form-name layout form-state coll props)
       (form-layout-grid* form-name layout {:form-data init-data} coll props))))

(defmulti template (fn [& args] (first args)))

(defmethod template :default [_ action options field-table]
           (template :basic action options field-table))

(defn form-header [form-title buttons]
  [:div {:class "form-header"}
   [:table
    [:tr
     [:td
      [:span {:class "form-title"} form-title]]
     [:td {:align "right"}
      (display-buttons "header" buttons)]]]])

(defn form-footer [buttons]
  [:div {:class "form-footer"}
   (display-buttons "footer" buttons)])

(defmethod template :over-under [_
                                 action
                                 {:keys [buttons title attrs]}
                                 field-table]
           (let [title (or title "Your Title Goes Here")
                 buttons (or buttons [[:submit] [:cancel]])]
             [:div {:class "sandbar-form"}
             (form-to [:post (cpath action) attrs]
                      (form-header title buttons)
                      field-table
                      (form-footer buttons))]))

(defmethod template :basic [_ action {:keys [buttons attrs]} field-div]
           (let [buttons (or buttons [[:submit] [:reset]])]
             [:div {:class "sandbar-form"}
              (form-to [:post (cpath action) attrs]
                       (append-buttons-to-table field-div buttons))]))

(defn- build-marshal-function
  "Build the default marshaling function."
  []
  (fn [params]
    (let [keys (map keyword (keys params))
          checkboxes (get-param params :__checkboxes)
          checkboxes (when checkboxes
                       (set (map keyword (if (sequential? checkboxes)
                                           checkboxes
                                           [checkboxes]))))
          marshal (fn [m] (dissoc m :submit :*))
          marshal (if checkboxes
                    (fn [m] (-> m
                                marshal
                                (dissoc :__checkboxes)
                                (get-yes-no-fields params checkboxes)))
                    
                    marshal)]
      (-> (get-params keys params)
          marshal
          clean-form-input))))

(defn- post-form
  "Handle a form post request."
  [request name validator options]
  (let [{:keys [on-success on-cancel marshal]} options
        {:keys [params]} request
        on-success (or on-success (constantly "/"))
        on-cancel (or on-cancel "/")
        marshal (or marshal
                    (build-marshal-function))]
    (redirect
     (if (form-cancelled? params)
       on-cancel
       (let [form-data (marshal params)
             failure (get (-> request :headers) "referer")]
         (if-valid validator form-data
                   on-success
                   (store-errors-and-redirect name failure)))))))

(defn- set-required
  "Use the provided validator to set which fields are required."
  [fields validator]
  (let [required (set (keys (validation-errors (validator {}))))]
    (vec (map #(if (contains? required (:field-name %))
                 (assoc % :required true)
                 %)
              fields))))

(defn- get-form
  "Handle a form get request."
  [request name form-data validator options]
  (let [{:keys [load defaults buttons title layout fields style properties]}
        options
        {:keys [params uri]} request
        properties (or properties {})
        load (or load (constantly nil))
        defaults (if defaults
                   (cond (map? defaults) defaults
                         (fn? defaults) (defaults request)
                         :else {})
                   {})
        id (get (:route-params request) "id")
        form-data (cond id (or form-data (load id))
                        defaults defaults
                        :else {})
        buttons (if buttons
                  {:buttons buttons}
                  {:buttons [[:submit] [:cancel]]})
        title (cond (string? title) title
                    (fn? title) (title request form-data)
                    :else nil)
        attrs (merge buttons
                     (when title {:title title}))
        layout (or layout [1])
        fields (-> (cond (vector? fields) fields
                         (fn? fields) (fields request form-data))
                   (set-required validator))
        style (or style :default)]
    (template style
              uri
              attrs
              (form-layout-grid layout
                                name
                                fields
                                request
                                form-data
                                properties))))

(defn make-form
  "Create a form handler function. The resulting function is a function of the
  request and optionally the form data. Options inculde:

  on-success: A function of the validated form data which returns the uri to be
              redirected to. Defaults to (constantly \"/\").
  on-cancel:  The uri to be redirected to when the cancel button in pushed.
              Defaults to \"/\".
  validator:  A function of a map which returns a validated map. Defaults to
              identity.
  marshal:    A function of the params which returns the form data. Defaults to
              getting all values as they appear in params and then running the
              data through clean-form-input.
  fields:     Either a vector of fields or a function of the request and form
              data which returns a vector of fields. Defaults to nil.
  load:       A function of the id that will load the form data. id is
              (get (:route-params request) 'id'). Defaults to a function that
              returns nil.
  defaults:   A map of the default form values or a function of the request
              which returns a map of default values.
  buttons:    A sequence of form buttons. Defaults to [[:submit] [:cancel]]
  style:      The form style. Defaults to :default but could also be :over-under
  title:      A function of the request and form data which returns the
              current form title. May also be a String. This in only used when
              the style requires a title.
  layout:     A vector which indicates the field layout. Defaults to [1].

  Example usage:

  (def simple-form
     (make-form :simple-form
       :fields [(textfield \"My Name\" :my-name)]
       :load #(fetch %)
       :on-success #(do (save %)
                        \"/landing/page/\")
       :on-cancel \"/landing/page\"))

  (defroutes billing-bol-routes
    (GET \"/simple/form/:id\" request (simple-form request))
    (GET \"/simple/form\" request (simple-form request))
    (POST \"/simple/form*\" request (simple-form request)))"
  [name & {:keys [validator] :as options}]
  (fn [request & form-data]
    (let [{:keys [request-method]} request
          validator (or validator identity)]
      (case request-method
            :post (post-form request name validator options)
            (get-form request name
                      (when form-data (first form-data))
                      validator
                      options)))))

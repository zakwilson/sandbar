;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.forms
  "Forms and form layouts."
  (:use [ring.util.response :only [redirect]]
        [compojure.core :only [routes GET POST]]
        [hiccup.form-helpers :only [form-to]]
        [sandbar.stateful-session :only [set-flash-value!
                                         get-flash-value]]
        [sandbar.core :only [cpath get-param property-lookup]]
        [sandbar.validation :only [if-valid required-fields]])
  (:require [compojure.route :as route]
            [clojure.string :as string]))

;;
;; Validation Helpers
;; ==================
;;

(defn store-errors-and-redirect [name redirect-page]
  (fn [form-data errors]
    (do (set-flash-value! name
                          (merge {:form-data form-data} errors))
        redirect-page)))

;;
;; Form Layout
;; ===========
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

(defn get-params
  "Get params from a map where the keys may be strings or keywords."
  [keys params]
  (reduce (fn [a b]
            (let [v (get-param params b)]
              (assoc a b v)))
          {}
          keys))

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

(defmulti template (fn [& args] (first args)))

(defmethod template :default [_ action options field-table]
           (template :basic action options field-table))

(defmethod template :over-under [_ action {:keys [buttons title]} field-table]
           (let [title (or title "Your Title Goes Here")
                 buttons (or buttons [[:submit] [:cancel]])]
             [:div {:class "sandbar-form"}
             (form-to [:post (cpath action)]
                      (form-header title buttons)
                      field-table
                      (form-footer buttons))]))

(defn get-colspan [rows]
  (let [cells (drop 1 (first rows))]
    (reduce (fn [colspan cell]
              (if (map? (second cell))
                (+ colspan (Integer/valueOf (or (:colspan (second cell)) 1)))
                (+ colspan 1)))
            0
            cells)))

(defn append-buttons-to-table [div buttons]
  (let [table (second div)
        colspan (get-colspan (rest table))
        new-table (conj table
                        [:tr 
                         [:td {:colspan colspan}
                          [:div {:class "buttons"}
                           (display-buttons "basic" buttons)]]])]
    (apply vector (first div) new-table (drop 2 div))))

(defmethod template :basic [_ action {:keys [buttons]} field-div]
           (let [buttons (or buttons [[:submit] [:reset]])]
             [:div {:class "sandbar-form"}
              (form-to [:post (cpath action)]
                       (append-buttons-to-table field-div buttons))]))

(defn form-cancelled?
  ([params]
     (form-cancelled? params "Cancel"))
  ([params value]
     (let [cancel-values (conj #{"cancel" "Cancel"} value)
           cancel (get-param params :cancel)]
       (contains? cancel-values cancel))))

(defn field-label [title key req]
  (let [title (if (map? title)
                (property-lookup title key)
                title)
        req (cond (keyword? req) req
                  (contains? (set req) key) :required
                  :else :optional)]
    [:div {:class "field-label"} title
     (if (= req :required) [:span {:class "required"} "*"] "")]))

(defn textarea
  ([title fname] (textarea title fname {} :optional))
  ([title fname options] (textarea title fname options :optional))
  ([title fname options req]
     {:type :textarea
      :label (field-label title fname req)
      :field-name fname
      :html [:textarea (merge {:name (name fname)} options)]}))

(defn textfield
  "Create a form text field. In each arity, title can be either a string or
   a map of keys to strings. If it is a map then the fname will be looked up
   in this map and the value will be used as the title. In the arity 3 version
   options can either be a map of options or the :required keyword."
  ([title fname] (textfield title fname {:size 35} :optional))
  ([title fname options] (if (map? options)
                           (textfield title fname options :optional)
                           (textfield title fname {:size 35} options)))
  ([title fname options req]
     {:type :textfield
      :label (field-label title fname req)
      :field-name fname
      :html [:input
             (merge {:type "Text" :name (name fname) :value ""
                     :class "textfield"} options)]}))

(defn password
  "Use textfield to create a text field and then change it to a
   password field."
  [& args]
  (let [textfield (apply textfield args)]
    (-> textfield
        (assoc :type :password)
        (assoc :html [:input (merge (last (:html textfield))
                                    {:type "Password"})]))))

(defn checkbox
  "Create a form checkbox. The title can be a map or a string. If it is a map
   then the displayed title will be looked up in the map using fname."
  ([title fname] (checkbox title fname {}))
  ([title fname options]
     {:type :checkbox
      :label [:span {:class "field-label"} (if (map? title)
                                             (property-lookup title fname)
                                             title)]
      :field-name fname
      :html [:input
             (merge {:type "checkbox"
                     :name (name fname)
                     :value "checkbox-true"} options)]}))

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

(defn wrap-checkboxes-in-group [coll]
  [:div {:class "group"}
     (map #(vector :div {:class "group-checkbox"} %) coll)])

(defn multi-checkbox
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

(defn filter-nil-vec [coll]
  (vec (filter #(not (nil? %)) coll)))

(defn get-multi-checkbox
  "Add the key k to the map m where the value of k is is a vector of
   selected values."
  [m params k all-values name-fn]
  (let [v (get-param params k)
        selected-values (set (filter-nil-vec (if (string? v) [v] v)))
        selected (filter #(contains? selected-values (name-fn %)) all-values)]
    (assoc m k selected)))

(defn checkbox? [field]
  (let [attrs (second field)]
    (= "checkbox" (:type attrs))))

(defn checkbox-group? [field]
  (let [attrs (second field)]
    (= "group" (:class attrs))))

(defn hidden [fname]
  {:type :hidden
   :label ""
   :field-name fname
   :html [:input {:type "hidden" :name (name fname) :value ""}]})

(defn select-map [coll key-key value-key]
  (apply merge (map #(sorted-map (key-key %) (value-key %)) coll)))

(defn select
  "Create a select form element."
  ([title fname coll kvs & optional]
     (let [key-and-value (first (dissoc kvs :prompt))
           prompt (first (:prompt kvs))
           k (key key-and-value)
           v (val key-and-value)
           opts (first (filter map? optional))
           req (first (filter #(not (map? %)) optional))
           s-map (select-map coll k v)
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
                (map #(vector :option {:value (key %)} (val %))
                     s-map)))
        :value-fn k})))

(defn multi-select [title fname coll kv & optional]
  (let [kv (dissoc kv :prompt)
        opts (merge {:multiple true :size 5} (first (filter map? optional)))
        req (first (filter #(not (map? %)) optional))]
    (select title fname coll kv opts req)))

(defn get-multi-select
  "Add the key k to the map m where the value of k is a vector of
   selected values."
  [m params k all-values name-fn]
  (get-multi-checkbox m params k all-values name-fn))

(defmulti set-form-field-value (fn [a b] (:type b)))

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
        value-set (set (if (coll? previous-val)
                         (map #((:value-fn input-field) %) previous-val)
                         [previous-val]))]
    (if (seq value-set)
      (assoc input-field :html
             (apply vector
                    (map #(if (and (vector? %)
                                   (= :option (first %))
                                   (contains? value-set (:value (second %))))
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

(defmulti create-form-field-cell (fn [_ m] (:type m)))

(defmethod create-form-field-cell :checkbox [form-state m]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec [:div
                                   (:html (set-form-field-value form-state m))
                                   label])]
    (if error-message
      [:div
       [:div {:class "error-message"} error-message]
       field-row]
      field-row)))

(defmethod create-form-field-cell :multi-checkbox [form-state m]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))]
    (if error-message
      [:div
       [:div {:class "error-message"} error-message]
       label field-row]
      [:div label field-row])))

(defmethod create-form-field-cell :default [form-state m]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))]
    (if error-message
      [:div
       label
       [:div {:class "error-message"} error-message]
       field-row]
      [:div label field-row])))

(defn- create-hidden-field [form-state m]
  (:html (set-form-field-value form-state m)))

(def one-column-layout (repeat 1))

;; TODO: Display all of the form messages. Not just the first one.
(defn form-layout-grid* [layout form-state coll]
  (let [div (if-let [form-errors (:form form-state)]
              [:div (let [messages (first form-errors)]
                      [:div {:class "warning"} messages])]
              [:div])
        the-form
        (conj div 
              (apply layout-table
                     layout
                     (map #(if-let [cell (create-form-field-cell form-state %)]
                             (vector cell)
                             nil)
                          (filter #(not (= (:type %) :hidden)) coll))))]
    (vec (concat the-form
                 (map #(create-hidden-field form-state %)
                      (filter #(= (:type %) :hidden) coll))))))

(defn form-layout-grid
  ([form-name coll request]
     (form-layout-grid one-column-layout form-name coll request {}))
  ([layout form-name coll request]
     (form-layout-grid layout form-name coll request {}))
  ([layout form-name coll request init-data]
     (if-let [form-state (get-flash-value form-name)]
       (form-layout-grid* layout form-state coll)
       (form-layout-grid* layout {:form-data init-data} coll))))

(defn nil-or-empty-string? [v]
  (or (not v)
      (and (string? v)
           (empty? v))))

;; TODO - This should preserve metadata

(defn clean-form-input
  "Set empty values to nil and remove the id if it is nil."
  [m]
  (apply merge
         (map #(hash-map (first %)
                         (let [value (last %)]
                           (if (nil-or-empty-string? value)
                             nil
                             value)))
              (if (not (nil-or-empty-string? (:id m)))
                m
                (dissoc m :id)))))

;;
;; Making forms easier
;;

(defn form-routes [resource-uri add-form edit-form submit-fn]
  (routes
   (GET (str resource-uri "/:id") request (edit-form request))
   (GET resource-uri request (add-form request))
   (POST resource-uri
         request
         (submit-fn request))))

(defn- field-def
  ([type name]
     (field-def type nil name nil))
  ([type props name attrs]
     (cond (nil? props) (list type name)
           (empty? attrs) (list type props name)
           :else (list type props name attrs))))

(defn- expand-field
  ([type props fields attrs]
     (cond (= (name type) "multi-checkbox")
           [(apply list type props fields)]
           (= (name type) "select")
           [(apply list type props (concat fields [attrs]))]
           (> (count fields) 1) (map #(field-def type props % attrs) fields)
           :else [(field-def type props (first fields) attrs)]))
  ([type props fields attrs required]
     (map #(concat % [required]) (expand-field type props fields attrs))))

(defn- expand [v p fields]
  (vec (apply concat
              (map #(let [t (first %)
                          r (rest %)
                          [f a] (if (map? (last r))
                                  [(butlast r) (last r)]
                                  [r {}])]
                      (cond (= (name t) "hidden")
                            [(field-def t (last %))]
                            (contains? #{"checkbox" "multi-checkbox"} (name t))
                            (expand-field t p f a)
                            :else (expand-field t p f a v)))
                   fields))))

(defmacro field-list [validator properties & fields]
  (let [v- (gensym "v_")
        expanded (expand v- properties fields)]
    `(let [~v- (required-fields ~validator)]
       ~expanded)))

(defn- categorize-fields [fields]
  (reduce (fn [m next]
            (let [type (name (first next))
                  r (rest next)]
              (cond (.endsWith type "multi-checkbox")
                    (assoc m :multi (conj (:multi m) r))
                    (.endsWith type "checkbox")
                    (assoc m :yes-no (concat (:yes-no m)
                                             (filter keyword? r)))
                    :else
                    (assoc m :default (concat (:default m)
                                              (filter keyword? r))))))
          {}
          fields))

(defn- build-marshal-chain [p fields]
  (let [categorized-fields (categorize-fields fields)
        chain [(list `get-params (vec (:default categorized-fields)) p)]
        chain (if-let [yes-no (:yes-no categorized-fields)]
                (conj chain (list `get-yes-no-fields p (set yes-no)))
                chain)
        chain (if-let [multi (:multi categorized-fields)]
                (apply conj chain
                       (map #(apply list `get-multi-checkbox p %) multi))
                chain)]
    (conj chain `clean-form-input)))

(defmacro defform
  "Define a form and produce a function that will generate the form's routes."
  [resource uri & {:keys [fields on-cancel on-success load]
                   :as options}]
  (let [resource-id (keyword (name resource))
        fields-sym (symbol (str (name resource) "-fields"))
        marshal-sym (symbol (str (name resource) "-marshal"))
        submit-sym (symbol (str (name resource) "-submit"))
        view-sym (symbol (str (name resource) "-view"))
        params- (gensym "params_")
        marshal-chain (build-marshal-chain params- fields)
        ;; semi-optional
        on-cancel (or on-cancel uri)
        on-success (or on-success `(fn [m#] ~uri))
        load (or load `(fn [id#] {}))
        ;; optional
        style (or (:style options) :default)
        validator (or (:validator options) `identity)
        properties (or (:properties options) {})
        title (or (:title options) `(fn [t#] (or (~resource-id ~properties)
                                                 "Form")))
        buttons (or (:buttons options) [[:submit "Submit"] [:cancel "Cancel"]])
        buttons (if (= (second (reverse buttons)) :labels)
                  buttons
                  (vec (concat buttons [:labels properties])))
        field-layout (or (:field-layout options) [1])]
    `(do
       (def ~fields-sym
            (field-list ~validator ~properties
                        ~@fields))
       (defn ~marshal-sym [~params-]
         (-> ~@marshal-chain))
       (defn ~submit-sym [request#]
         (let [params# (:params request#)
               uri# (:uri request#)
               {cancel-val# :cancel and-new-val# :save-and-new}
               (special-button-text ~buttons)]
           (redirect
            (if (form-cancelled? params# cancel-val#)
              ~on-cancel
              (let [form-data# (~marshal-sym params#)
                    submit# (get-param params# :submit)]
                (if-valid ~validator form-data#
                          (fn [m#]
                            (let [s# (~on-success m#)]
                              (if (and and-new-val# (= submit# and-new-val#))
                                uri#
                                s#)))
                          (store-errors-and-redirect ~resource-id uri#)))))))
       (defn ~view-sym [type# request#]
         (let [params# (:params request#)
               form-data# (case type#
                                :edit (let [id# (get-param params# :id)]
                                        (~load id#))
                                {})]
           (template ~style
                     ~uri
                     {:title (~title type#)
                      :buttons ~buttons}
                     (form-layout-grid ~field-layout
                                       ~resource-id
                                       ~fields-sym
                                       request#
                                       form-data#))))
       (defn ~resource [layout#]
         (form-routes ~uri
                      (fn [request#]
                        (layout# request# (~view-sym :add request#)))
                      (fn [request#]
                        (layout# request# (~view-sym :edit request#)))
                      ~submit-sym)))))

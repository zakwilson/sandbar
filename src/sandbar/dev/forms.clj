;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.forms
  "High level form library. Form components and layout."
  (:use (hiccup core form-helpers)
        (sandbar core util stateful-session)))

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

(defn get-params [key-map params]
  (reduce (fn [a b] (assoc a (keyword b) (get params b)))
          {}
          (map name key-map)))

(defn cancel-button []
  [:input {:type "submit" :value "Cancel" :name "cancel"
           :class "sandbar-button"}])

(defn new-submit-button
  ([] (new-submit-button "submit"))
  ([v]
     [:input {:type "submit" :value v :name "submit"
              :class "sandbar-button"}]))

(defn new-reset-button [v]
  [:input {:type "reset" :value v
           :class "sandbar-button"}])

(defn alt-submit-button [text]
  [:input {:type "submit" :value text :name "submit"
           :class "sandbar-button"}])

(defn create-submit-button [[k v]]
  (cond (= k :submit) (new-submit-button v)
        :else (new-submit-button v)))

(defn submit-and-cancel-buttons [horf submit-buttons]
  (let [submit-spec (if (map? submit-buttons)
                      submit-buttons
                      {:submit submit-buttons})]
    (vec
     (concat
      [:span {:class (str horf "-buttons")}]
      (interleave (map create-submit-button submit-spec) (cycle ["&nbsp;"]))
      [(cancel-button)]))))

(defn form-header [form-title submit-buttons]
  [:div {:class "form-header"}
   [:table
    [:tr
     [:td
      [:span {:class "form-title"} form-title]]
     [:td {:align "right"}
      (submit-and-cancel-buttons "header" submit-buttons)]]]])

(defn form-footer [submit-buttons]
  [:div {:class "form-footer"}
   (submit-and-cancel-buttons "footer" submit-buttons)])

(defn standard-form [title action submit-name body]
  [:div {:class "sandbar-form"}
   (form-to [:post (cpath action)]
           (form-header title submit-name)
           body
           (form-footer submit-name))])

(defn login-form [action submit-name body]
  [:div {:class "sandbar-form"}
   (form-to [:post (cpath action)]
            [:div {:class "login-form"}
             body
             [:div {:class "login-buttons"}
              (new-submit-button submit-name)
              "&nbsp;&nbsp;"
              (new-reset-button "Reset")]])])

(defn form-cancelled? [params]
  (= "Cancel" (get params "cancel")))

(defn form-field-label [title req]
  [:div {:class "field-label"} title
   (if (= req :required) [:span {:class "required"} "*"] "")])

(defn form-textarea
  ([title fname options] (form-textarea title fname options :optional))
  ([title fname options req]
     {:type :textarea
      :label (form-field-label title req)
      :field-name fname
      :html [:textarea (merge {:name (name fname)} options)]}))

(defn form-textfield
  "Create a form text field. In each arity, title can be either a string or
   a map of keys to strings. If it is a map then the fname will be looked up
   in this map and the value will be used as the title. In the arity 3 version
   options can either be a map of options or the :required keyword."
  ([title fname] (form-textfield title fname {:size 35} :optional))
  ([title fname options] (if (keyword? options)
                           (form-textfield title fname {:size 35} options)
                           (form-textfield title fname options :optional)))
  ([title fname options req]
     {:type :textfield
      :label (form-field-label (if (map? title)
                                 (property-lookup title fname)
                                 title) req)
      :field-name fname
      :html [:input
              (merge {:type "Text" :name (name fname) :value ""
                      :class "sandbar-textfield"} options)]}))

(defn form-password
  "Use form-textfield to create a text field and then change it to a
   password field."
  [& args]
  (let [textfield (apply form-textfield args)]
    (-> textfield
        (assoc :type :password)
        (assoc :html [:input (merge (last (:html textfield))
                                    {:type "Password"})]))))

(defn form-checkbox
  "Create a form checkbox. The title can be a map or a string. If it is a map
   then the displayed title will be looked up in the map using fname."
  ([title fname] (form-checkbox title fname {}))
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
                   (if (and (contains? cb-set (keyword (key b)))
                            (= "checkbox-true" (val b)))
                     (assoc a (keyword (key b)) "Y")
                     a))
                 m
                 params)]
    (reduce (fn [a b] (if (b a) a (assoc a b "N")))
            new-map
            cb-set)))

(defn wrap-checkboxes-in-group [coll]
  [:div {:class "group"}
     (map #(vector :div {:class "group-checkbox"} %) coll)])

(defn form-multi-checkbox
  ([props many-spec]
     (form-multi-checkbox props
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
  (let [v (get params (name k))
        selected-values (set (filter-nil-vec (if (string? v) [v] v)))
        selected (filter #(contains? selected-values (name-fn %)) all-values)]
    (assoc m k selected)))

(defn checkbox? [field]
  (let [attrs (second field)]
    (= "checkbox" (:type attrs))))

(defn checkbox-group? [field]
  (let [attrs (second field)]
    (= "group" (:class attrs))))

(defn form-hidden [fname]
  {:type :hidden
   :label ""
   :field-name fname
   :html [:input {:type "hidden" :name (name fname) :value ""}]})

(defn select-map [coll key-key value-key]
  (apply merge (map #(sorted-map (key-key %) (value-key %)) coll)))

(defn form-select
  ([title fname k v coll opts top]
     (form-select title fname k v coll opts top :optional))
  ([title fname k v coll opts top req]
     (let [s-map (select-map coll k v)]
       {:type :select
        :label (form-field-label title req)
        :field-name fname
        :html (vec
               (concat
                [:select (merge {:name (name fname)} opts)]
                [[:option {:value (key (first top))} (val (first top))]] 
                (map #(vector :option {:value (key %)} (val %))
                     s-map)))})))

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
        previous-val ((keyword field-name) (:form-data form-state))]
    (if previous-val
      (apply vector
             (map #(if (and (vector? %)
                            (= :option (first %))
                            (= previous-val (:value (second %))))
                     [:option {:value previous-val :selected "selected"}
                      (last %)]
                     %)
                  input-field))
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
     (if-let [form-state (get-flash-value! form-name)]
       (form-layout-grid* layout form-state coll)
       (form-layout-grid* layout {:form-data init-data} coll))))

;; TODO - This should preserve metadata

(defn clean-form-input
  "Set empty values to nil and remove the id if it is nil."
  [m]
  (apply merge
         (map #(hash-map (first %)
                         (let [value (last %)]
                           (if (and (not (keyword? value))
                                    (empty? value))
                             nil
                             value)))
              (if (and (:id m)
                       (not (empty? (:id m))))
                m
                (dissoc m :id)))))


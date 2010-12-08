;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test.forms
  (:use [clojure.test :only [deftest testing]]
        [sandbar.forms]
        [sandbar.stateful-session :only []]))

;;
;; Fixtures
;;

(defn create-test-multi-checkbox [fname coll]
  {:type :multi-checkbox
   :field-name fname
   :label ""
   :html (wrap-checkboxes-in-group
          (map
           #(let [value (first %)
                  on-off (last %)
                  attrs {:type "checkbox" :name fname :value value}
                  attrs (if (= on-off :-)
                          attrs
                          (assoc attrs :checked "true"))]
              [:input attrs value])
           (partition 2 coll)))
   :value-fn :id})

(defn button-row [buttons]
  [:tr 
   [:td {:colspan 1}
    [:div.buttons
     (display-buttons "basic" buttons)]]])

(defn test-form-opt-label [field-name]
  [:div {:class "field-label"} field-name ""])

(defn test-form-req-label [field-name]
  [:div {:class "field-label"} field-name
   [:span {:class "required"} "*"]])

(defn form-textfield-fixture [field-name value]
  [:input {:size 35 :type "Text" :name field-name :value value
           :class "textfield"}])

(defn form-hidden-fixture [field-name value]
  [:input {:type "hidden" :name field-name :value value}])

(defn form-password-fixture [field-name value]
  [:input {:size 35 :type "Password" :name field-name :value value
           :class "textfield"}])

(defmulti field-fixture (fn [& args] (first args)))

(defmethod field-fixture :textfield [_ label name & {:keys [value flags]}]
           (let [field-value (or value "")]
             [:div.sandbar-field
              (if (contains? flags :required)
                (test-form-req-label label)
                (test-form-opt-label label))
              [:div.error-message {:style "display:none;"}]
              (form-textfield-fixture name field-value)]))

(defn form-checkbox-fixture [field-name value]
  [:input {:type "checkbox" :name field-name :value "checkbox-true"}])

(defn form-checkbox-label-fixture [title]
  [:span {:class "field-label"} title])

(defn test-form-multi-checkbox [field-name value]
  [:div [:input {:type "Text", :name field-name, :value value}]])

(defn test-form-textarea [field-name value]
  (if (empty? value)
    [:textarea {:name field-name}]
    [:textarea {:name field-name} value]))

(def td-std-opts {:valign "top"})

;;
;; Utilities
;;

#_(deftest get-params-test
  (is (= (get-params [:a :b] {"a" "a" "b" "b"})
         {:a "a" :b "b"}))
  (is (= (get-params [:a :b] {:a "a" :b "b"})
         {:a "a" :b "b"}))
  (is (= (get-params [:a :b] {"c" "c" :a "a" :b "b"})
         {:a "a" :b "b"}))
  (is (= (get-params [:a :b :c :d :e] {:a "a" :b "2" :c "3.4" :d "5t"
                                       "e" "3.5t"})
         {:a "a" :b 2 :c 3.4 :d "5t" :e "3.5t"}))
  (is (= (get-params [:a :b] {:a ["1" "2" "3.5"] :b ["a" "2" "4.5t"]})
         {:a [1 2 3.5] :b ["a" 2 "4.5t"]})))

#_(deftest clean-form-input-test
  (is (= (clean-form-input {:id "" :name "" :age 10})
         {:name nil :age 10}))
  (is (= (meta (clean-form-input
                (with-meta {:id "" :name "" :age 10} {:a "a"})))
         {:a "a"})))

;;
;; Form Elements
;;

#_(deftest textfield-test
  (are [args _ e-label exp-name exp-value]
       (= (apply textfield args)
          {:type :textfield
           :label e-label
           :field-name :name
           :html (form-textfield-fixture exp-name exp-value)})

       ["f1" :name] :=> (test-form-opt-label "f1") "name" ""

       ["f1" :name :required] :=> (test-form-req-label "f1") "name" ""

       ["f1" :name {:size 35}] :=> (test-form-opt-label "f1") "name" ""

       [{:name "Name"} :name] :=> (test-form-opt-label "Name") "name" ""

       [{:name "Name"} :name {:size 35}] :=>
       (test-form-opt-label "Name") "name" ""

       ["f1" :name {:size 35} :required] :=>
       (test-form-req-label "f1") "name" ""

       [{:name "Name"} :name {:size 35} :required] :=>
       (test-form-req-label "Name") "name" ""))

#_(deftest password-test
  (are [args _ e-label exp-name exp-value]
       (= (apply password args)
          {:type :password
           :label e-label
           :field-name :name
           :html (form-password-fixture exp-name exp-value)})

       ["f1" :name] :=> (test-form-opt-label "f1") "name" ""

       ["f1" :name :required] :=> (test-form-req-label "f1") "name" ""

       ["f1" :name {:size 35}] :=> (test-form-opt-label "f1") "name" ""

       [{:name "Name"} :name] :=> (test-form-opt-label "Name") "name" ""

       [{:name "Name"} :name {:size 35}] :=>
       (test-form-opt-label "Name") "name" ""

       ["f1" :name {:size 35} :required] :=>
       (test-form-req-label "f1") "name" ""

       [{:name "Name"} :name {:size 35} :required] :=>
       (test-form-req-label "Name") "name" ""))

#_(deftest checkbox-test
  (are [args _ e-label exp-name exp-value]
       (= (apply checkbox args)
          {:type :checkbox
           :label e-label
           :field-name :name
           :html (form-checkbox-fixture exp-name exp-value)})

       ["f1" :name] :=> (form-checkbox-label-fixture "f1") "name" false
       
       [{:name "Name"} :name] :=>
       (form-checkbox-label-fixture "Name") "name" false

       ["f1" :name {}] :=>
       (form-checkbox-label-fixture "f1") "name" false

       [{:name "Name"} :name {}] :=>
       (form-checkbox-label-fixture "Name") "name" false))

#_(deftest select-test
  (let [coll [{:a 1 :b "a"} {:a 2 :b "b"}]]
    (is (= (:html (select "T" :test coll {:a :b}))
           [:select {:name "test"}
            [:option {:value 1} "a"]
            [:option {:value 2} "b"]]))))
;;
;; Buttons
;;

#_(deftest test-append-buttons-to-table
  (let [button [[:save]]
        row (button-row button)
        t [:table [:tr [:td]]]
        bt [:table [:tr [:td]] row]]
    (is (= (append-buttons-to-table [:div t [:input]] button)
           [:div bt [:input]]))
    (is (= (append-buttons-to-table [:div [:input] t [:input]] button)
           [:div [:input] bt [:input]]))
    (is (= (append-buttons-to-table [:div [:input] t] button)
           [:div [:input] bt]))
    (is (= (append-buttons-to-table [:div [:input]] button)
           [:div [:input]]))))

;;
;; Marshal and Binding data
;;

#_(deftest test-get-yes-no-fields
  (t "get yes/no fields"
     (t "when the field is selected"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:a})
               {:a "Y"})))
     (t "when the field is not selected"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:b})
               {:b "N"})))
     (t "when one field is selected and one is not"
        (is (= (get-yes-no-fields {}
                                  {:a "checkbox-true"}
                                  #{:a :b})
               {:a "Y" :b "N"})))))

#_(deftest test-get-multi-checkbox
  (t "get multi checkbox"
     (t "when one item is seleted"
        (is (= (get-multi-checkbox {}
                                   {"a" "a"}
                                   :a
                                   [{:id 1 :name "a"} {:id 2 :name "b"}]
                                   :name)
               {:a [{:id 1 :name "a"}]})))
     (t "when two items are seleted"
        (is (= (get-multi-checkbox {}
                                   {"a" ["a" "b"]}
                                   :a
                                   [{:id 1 :name "a"} {:id 2 :name "b"}]
                                   :name)
               {:a [{:id 1 :name "a"} {:id 2 :name "b"}]})))
     (t "when nothing is seleted"
        (is (= (get-multi-checkbox {}
                                   {}
                                   :a
                                   [{:id 1 :name "a"} {:id 2 :name "b"}]
                                   :name)
               {:a []})))))

#_(deftest set-form-field-value:multi-checkbox-test
  (t "set multi checkbox field value"
     (t "when one of two checkboxes are selected"
        (is (= (set-form-field-value
                {:form-data {:test [{:id "a"}]}} 
                (create-test-multi-checkbox :test ["a" :- "b" :-]))
               (create-test-multi-checkbox :test ["a" :+ "b" :-]))))
     (t "when two of two checkboxes are selected"
        (is (= (set-form-field-value
                {:form-data {:test [{:id "a"} {:id "b"}]}} 
                (create-test-multi-checkbox :test ["a" :- "b" :-]))
               (create-test-multi-checkbox :test ["a" :+ "b" :+]))))))

#_(deftest set-form-field-value:select-test
  (is (= (:html
          (set-form-field-value
           {:form-data {:test 1}}
           (select "A" :test [{:a 1 :n "a"} {:a 2 :n "b"}] {:a :n})))
         [:select {:name "test"}
          [:option {:value 1 :selected "selected"} "a"]
          [:option {:value 2} "b"]])))

;;
;; Layout
;;

#_(deftest test-layout-table
  (t "create a layout table"
     (t "with one cell"
        (is (= (layout-table [1] "A")
               [:table [:tr [:td td-std-opts "A"]]])))
     (t "with one cell on each row"
        (is (= (layout-table [1 1] "A" "B")
               [:table
                [:tr [:td td-std-opts "A"]]
                [:tr [:td td-std-opts "B"]]])))
     (t "with two things going into one row"
        (is (= (layout-table [1] ["A" "B"])
               [:table
                [:tr [:td td-std-opts "A" "B"]]])))
     (t "with a vector of two things going into one cell"
        (is (= (layout-table [1] [["A" "B"]])
               [:table
                [:tr [:td td-std-opts ["A" "B"]]]])))
     (t "with two things going into one cell and then two things in two cells"
        (is (= (layout-table [1 2] ["A" "B"] "C" "D")
               [:table
                [:tr [:td {:colspan 2 :valign "top"} "A" "B"]]
                [:tr [:td td-std-opts "C"] [:td td-std-opts "D"]]])))
     (t "with two cells in one row"
        (is (= (layout-table [2] "A" "B")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]])))
     (t "with two cells on each of two rows"
        (is (= (layout-table [2 2] "A" "B" "C" "D")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]
                [:tr [:td td-std-opts "C"] [:td td-std-opts "D"]]])))
     (t "with two cells on the first row and one cell on the second"
        (is (= (layout-table [2 1] "A" "B" "C")
               [:table
                [:tr [:td td-std-opts "A"] [:td td-std-opts "B"]]
                [:tr [:td {:colspan 2 :valign "top"} "C"]]])))
     (t "with one cell on the first row and two cells on the second"
        (is (= (layout-table [1 2] "A" "B" "C")
               [:table
                [:tr [:td {:colspan 2 :valign "top"} "A"]]
                [:tr [:td td-std-opts "B"] [:td td-std-opts "C"]]])))
     (t "with a pyramid layout"
        (is (= (layout-table [1 2 3 2 1] "A" "B" "C" "D" "E" "F" "G" "H" "I")
               [:table
                [:tr [:td {:colspan 3 :valign "top"} "A"]]
                [:tr [:td td-std-opts "B"] [:td {:colspan 2 :valign "top"} "C"]]
                [:tr
                 [:td td-std-opts "D"]
                 [:td td-std-opts "E"]
                 [:td td-std-opts "F"]]
                [:tr [:td td-std-opts "G"] [:td {:colspan 2 :valign "top"} "H"]]
                [:tr [:td {:colspan 3 :valign "top"} "I"]]])))
     (t "will a nil cell value"
        (is (= (layout-table [] "A" nil)
               [:table
                [:tr [:td {:valign "top"} "A"]]])))))

(defn test-form-table [layout & cell-values]
  (vec
   (conj [:div] (apply layout-table layout cell-values))))

#_(deftest test-form-layout
  (binding [sandbar-session (atom {})]
    (t "create form layout"
       (t "with a single required text field and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(textfield "f1" :name :required)]
                  {})
                 (test-form-table [1]
                                  [(field-fixture :textfield "f1" "name"
                                                  :flags #{:required})]))))
       (t "with a single optional text field and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(textfield "f1" :name)]
                  {})
                 (test-form-table [1]
                                  [(field-fixture :textfield "f1" "name")]))))
       (t "with a single optional text field and an initial state"
          (is (= (form-layout-grid
                  one-column-layout
                  :test
                  [(textfield "f1" :name)]
                  {}
                  {:name "n"})
                 (test-form-table [1]
                                  [(field-fixture :textfield "f1" "name"
                                                  :value "n")]))))
       (t "with two optional text fields and no initial state"
          (is (= (form-layout-grid
                  :test
                  [(textfield "f1" :name)
                   (textfield "f2" :age)]
                  {})
                 (test-form-table [1 1]
                                  [(field-fixture :textfield "f1" "name")]
                                  [(field-fixture :textfield "f2" "age")]))))
       (t "explicitly using one column layout - layout vec has 2 of 4 values"
          (is (= (form-layout-grid
                  one-column-layout
                  :test
                  [(textfield "f1" :name)
                   (textfield "f2" :age)]
                  {})
                 (test-form-table [1 1]
                                  [(field-fixture :textfield "f1" "name")]
                                  [(field-fixture :textfield "f2" "age")]))))
       (t "using a two column layout"
          (is (= (form-layout-grid
                  [2]
                  :test
                  [(textfield "f1" :name)
                   (textfield "f2" :age)]
                  {})
                 (test-form-table [2]
                                  [(field-fixture :textfield "f1" "name")]
                                  [(field-fixture :textfield "f2" "age")]))))
       (t "using a mix of one and two columns"
          (is (= (form-layout-grid
                  [1 2]
                  :test
                  [(textfield "f1" :name)
                   (textfield "f2" :age)
                   (textfield "f3" :title)]
                  {})
                 (test-form-table [1 2]
                                  [(field-fixture :textfield "f1" "name")]
                                  [(field-fixture :textfield "f2" "age")]
                                  [(field-fixture :textfield "f3" "title")]))))
       (t "with one hidden field"
          (is (= (form-layout-grid
                  :test
                  [(hidden :title)
                   (textfield "f1" :name)
                   (textfield "f2" :age)]
                  {})
                 [:div
                  (layout-table []
                                [(field-fixture :textfield "f1" "name")]
                                [(field-fixture :textfield "f2" "age")])
                  (form-hidden-fixture "title" "")])))
       (t "with two hidden fields"
          (is (= (form-layout-grid
                  :test
                  [(hidden :title)
                   (hidden :id)
                   (textfield "f1" :name)
                   (textfield "f2" :age)]
                  {})
                 [:div
                  (layout-table []
                                [(field-fixture :textfield "f1" "name")]
                                [(field-fixture :textfield "f2" "age")])
                  (form-hidden-fixture "title" "")
                  (form-hidden-fixture "id" "")]))))))

#_(deftest test-create-form-field-cell
  (t "create form field cell"
     (t "containing a text field"
        (is (= (create-form-field-cell
                {}
                (textfield "f1" :name))
               (field-fixture :textfield "f1" "name"))))
     (t "containing a text area"
        (is (= (create-form-field-cell
                {}
                (textarea "f1" :name {}))
               [:div.sandbar-field
                (test-form-opt-label "f1")
                [:div.error-message {:style "display:none;"}]
                (test-form-textarea "name" "")])))
     (t "containing a checkbox"
        (is (= (create-form-field-cell
                {}
                (checkbox "f1" :name))
               [:div
                (form-checkbox-fixture "name" false)
                [:span {:class "field-label"} "f1"]])))
     (t "containing a text checkbox group"
        (is (= (create-form-field-cell
                {}
                (multi-checkbox {}
                                     :name [{:id 1 :value "a"}
                                            {:id 2 :value "b"}]
                                     :value))
               [:div
                [:span {:class "group-title"} "name"]
                [:div {:class "group"}
                 [[:div {:class "group-checkbox"}
                   [:input {:type "checkbox", :name :name, :value "a"} "a"]]
                  [:div {:class "group-checkbox"}
                   [:input {:type "checkbox", :name :name, :value "b"} "b"]]]]])))))

;;
;; Macros
;;

#_(deftest test-expand
  (t "expand a form field list"
     (let [expand #'sandbar.forms/expand]
       (t "for hidden fields"
          (is (= (expand 'v {} ['(hidden :t)])
                 ['(hidden :t)])))
       (t "for checkboxes"
          (is (= (expand 'v {} ['(checkbox :t)])
               ['(checkbox {} :t)]))
          (is (= (expand 'v {} ['(checkbox :t {:class :x})])
                 ['(checkbox {} :t {:class :x})])))
       (t "for multi-checkboxes"
          (is (= (expand 'v {} ['(multi-checkbox :t [] name)])
               ['(multi-checkbox {} :t [] name)])))
       (t "for textfields"
          (is (= (expand 'v {} ['(textfield :t)])
                 ['(textfield {} :t v)]))
          (is (= (expand 'v {} ['(textfield :t {:class :x})])
                 ['(textfield {} :t {:class :x} v)]))
          (is (= (expand 'v {} ['(textfield "a" :t)])
                 ['(textfield "a" :t v)]))
          (is (= (expand 'v {} ['(textfield "a" :t {:class :x})])
                 ['(textfield "a" :t {:class :x} v)])))
       (t "for passwords"
          (is (= (expand 'v {} ['(password :t)])
               ['(password {} :t v)]))
          (is (= (expand 'v {} ['(password :t {:class :x})])
                 ['(password {} :t {:class :x} v)])))
       (t "for textareas"
          (is (= (expand 'v {} ['(textarea :t)])
               ['(textarea {} :t v)]))
          (is (= (expand 'v {} ['(textarea :t {:class :x})])
                 ['(textarea {} :t {:class :x} v)])))
       (t "for selects"
          (is (= (expand 'v {} ['(select :t [] {:i :v})])
               ['(select {} :t [] {:i :v} v)]))
          (is (= (expand 'v {} ['(select :t [] {:i :v} {:class :x})])
                 ['(select {} :t [] {:i :v} {:class :x} v)])))
       (t "for multi-selects"
          (is (= (expand 'v {} ['(multi-select :t [] {:i :v})])
               ['(multi-select {} :t [] {:i :v} v)]))
          (is (= (expand 'v {} ['(multi-select :t [] {:i :v} {:class :x})])
                 ['(multi-select {} :t [] {:i :v} {:class :x} v)]))))))


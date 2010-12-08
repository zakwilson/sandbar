;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test.validation
  (:use [clojure.test :only [deftest testing is]]
        [sandbar.validation]))

(deftest test-add-validation-errors
  (testing "add validation error"
     (testing "when errors are empt"
        (is (= (add-validation-error {} :a "a")
               {:_validation-errors {:a ["a"]}})))
     (testing "when no key is passed, error goes in :form key"
        (is (= (add-validation-error {} "a")
               {:_validation-errors {:form ["a"]}})))
     (testing "when error key exists in form data"
        (is (= (add-validation-error {:a "a"} :a "a")
               {:a "a" :_validation-errors {:a ["a"]}} )))
     (testing "when validation errors exist and putting new value in same key"
        (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}}
                                     :a "b")
               {:a "a" :_validation-errors {:a ["a" "b"]}})))
     (testing "when validation errors exist and adding new error"
        (is (= (add-validation-error {:a "a" :_validation-errors {:a ["a"]}}
                                     :b "b")
               {:a "a" :_validation-errors {:a ["a"] :b ["b"]}})))))

(defn example-validator
  "A validator takes a map as an argument and then returns the map with a
   key named :_validation-errors that contains a map or error messages."
  [m]
  (if (= (:a m) "a")
    m
    (add-validation-error m :a ":a does not contain the letter a")))

(deftest test-if-valid
  (testing "testing if-valid"
     (testing "with built-in functions and validation is true"
          (is (= (if-valid identity {:a "a"} identity list)
                 {:a "a"})))
     (testing "with custom functions"
        (let [success (fn [a] (:a a))
              failure (fn [a b] (first (:a b)))]
          (testing "when validation is true"
             (is (= (if-valid example-validator {:a "a"}
                              success
                              failure)
                    "a")))
          (testing "when validation is false"
             (is (= (if-valid example-validator {:a "b"}
                              success
                              failure)
                    ":a does not contain the letter a")))))))

(deftest test-non-empty-string
  (testing "test non-empty string"
     (testing "when string is empty"
        (is (= (non-empty-string {:a ""} :a "error")
               {:a "" :_validation-errors {:a ["error"]}}))
        (testing "with map message"
           (is (= (non-empty-string {:a ""} :a {:a "Username"})
                  {:a "" :_validation-errors
                   {:a ["Username cannot be blank!"]}})))
        (testing "with empty message"
           (is (= (non-empty-string {:a ""} :a)
                  {:a "" :_validation-errors
                   {:a ["a cannot be blank!"]}})))
        (testing "with custom error message"
           (is (= (non-empty-string {:a ""} :a {:a-validation-error "x"})
                  {:a "" :_validation-errors
                   {:a ["x"]}}))))
     (testing "when nil instead of string"
        (is (= (non-empty-string {:a nil} :a "error")
               {:a nil :_validation-errors {:a ["error"]}})))
     (testing "when value is non-empty string"
        (is (= (non-empty-string {:a "a"} :a "error")
               {:a "a"})))
     (testing "with multiple keywords"
        (testing "where validation passes"
           (is (= (non-empty-string {:a "a" :b "b"} :a :b "error")
                  {:a "a" :b "b"})))
        (testing "where validation fails for one keyword"
           (is (= (non-empty-string {:a "a" :b ""} :a :b "error")
                  {:a "a" :b "" :_validation-errors {:b ["error"]}})))
        (testing "where validation fails for one keyword and no error message"
           (is (= (non-empty-string {:a "a" :b ""} :a :b)
                  {:a "a" :b ""
                   :_validation-errors {:b ["b cannot be blank!"]}})))
        (testing "where validation fails for one keyword with properties"
           (is (= (non-empty-string {:a "a" :b ""} :a :b {:b "b"})
                  {:a "a" :b ""
                   :_validation-errors {:b ["b cannot be blank!"]}})))
        (testing "where validation fails for all keywords with properties"
           (is (= (non-empty-string {:a "" :b ""} :a :b {:a "a" :b "b"})
                  {:a "" :b ""
                   :_validation-errors {:b ["b cannot be blank!"]
                                        :a ["a cannot be blank!"]}}))))))

(deftest test-build-validator
  (testing "build validator"
     (testing "out of one validator fn and validation passes"
        (is (= ((build-validator (non-empty-string :a)) {:a "a"})
               {:a "a"})))
     (testing "out of one validator fn and validation fails"
        (is (= ((build-validator (non-empty-string :a)) {:a ""})
               {:a ""
                :_validation-errors {:a ["a cannot be blank!"]}})))
     (testing "out of two validators fns and validation passes"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)) {:a "a" :b "b"})
               {:a "a" :b "b"})))
     (testing "out of two validators fns and validation fails for one"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)) {:a "a" :b ""})
               {:a "a" :b ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (testing "out of three validators with an ensure, all validations pass"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "b" :c "c"})
               {:a "a" :b "b" :c "c"})))
     (testing "out of four validators with an ensure in the middle, all pass"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d))
                {:a "a" :b "b" :c "c" :d "d"})
               {:a "a" :b "b" :c "c" :d "d"})))
     (testing "error after :ensure is not detected if there is error before"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "" :c ""})
               {:a "a" :b "" :c ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (testing "error after :ensure is detected if no error before"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)) {:a "a" :b "b" :c ""})
               {:a "a" :b "b" :c ""
                :_validation-errors {:c ["c cannot be blank!"]}})))
     (testing "thee levels of nesting"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "c" :d "d" :e "e" :f "f"})
               {:a "a" :b "b" :c "c" :d "d" :e "e" :f "f"})))
     (testing "errors in first group hides other errors"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "" :c "" :d "" :e "" :f ""})
               {:a "a" :b "" :c "" :d "" :e "" :f ""
                :_validation-errors {:b ["b cannot be blank!"]}})))
     (testing "errors in second group hides third group's errors"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "" :d "d" :e "" :f ""})
               {:a "a" :b "b" :c "" :d "d" :e "" :f ""
                :_validation-errors {:c ["c cannot be blank!"]}})))
     (testing "find errors in third group"
        (is (= ((build-validator (non-empty-string :a)
                                 (non-empty-string :b)
                                 :ensure
                                 (non-empty-string :c)
                                 (non-empty-string :d)
                                 :ensure
                                 (non-empty-string :e)
                                 (non-empty-string :f))
                {:a "a" :b "b" :c "c" :d "d" :e "" :f ""})
               {:a "a" :b "b" :c "c" :d "d" :e "" :f ""
                :_validation-errors {:e ["e cannot be blank!"]
                                     :f ["f cannot be blank!"]}})))))


(ns sandbar.example.forms.complex-test
  (:use (clojure test)
        (sandbar [test :only (t)]
                 validation)
        (sandbar.example.forms complex)))

(def valid-data {:notes "a"
                 :last-name "a"
                 :first-name "a"
                 :roles [:user]
                 :username "a"
                 :email "a@b.com"
                 :account-enabled "Y"
                 :region 1
                 :languages [{:id 1 :name "c"}]
                 :password "abcdefghij"})

(deftest validator-test
  (t "test validator"
     (t "with valid data"
        (is (= (validator valid-data)
               valid-data)))
     (t "with no username selected"
        (let [d (dissoc valid-data :username)]
          (is (= (validation-errors (validator d))
                 {:username ["Username cannot be blank!"]}))))
     (t "with an invalid username selected"
        (let [d (assoc valid-data :username 10)]
          (is (= (validation-errors (validator d))
                 {:username ["Username cannot be blank!"]}))))
     (t "with a empty list of maps"
        (let [d (assoc valid-data :languages [])]
          (is (= (validation-errors (validator d))
                 {:languages ["Languages must be a list of maps!"]}))))
     (t "with an empty map in the list"
        (let [d (assoc valid-data :languages [{}])]
          (is (= (validation-errors (validator d))
                 {:languages ["Languages must be a list of maps!"]}))))
     (t "with an invalid map in the list"
        (let [d (assoc valid-data :languages [{:id 2 :name 8}])]
          (is (= (validation-errors (validator d))
                 {:languages ["Languages must be a list of maps!"]}))))
     (t "with no region selected"
        (let [d (dissoc valid-data :region)]
          (is (= (validation-errors (validator d))
                 {:region ["Region must be an integer number!"]}))))
     (t "with an invalid region"
        (let [d (assoc valid-data :region "7")]
          (is (= (validation-errors (validator d))
                 {:region ["Region must be an integer number!"]}))))))

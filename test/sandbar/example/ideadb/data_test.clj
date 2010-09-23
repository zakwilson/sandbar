(ns sandbar.example.ideadb.data-test
  (:use (clojure test)
        (sandbar [test :only (t)])
        (sandbar.dev tables)
        (sandbar.example.ideadb data)))

(deftest test-carte-table-adapter
  (t "test carte table adapter"
     (t "with minimum query"
        (is (= (carte-table-adapter :t {} {})
               [:t])))
     (t "with one filter"
        (is (= (carte-table-adapter :t {:a "a"} {})
               [:t {:a "a"}])))
     (t "with one filter and empty sort"
        (is (= (carte-table-adapter :t {:a "a"} {:sort []})
               [:t {:a "a"}])))
     (t "with one filter and a sort"
        (is (= (carte-table-adapter :t {:a "a"} {:sort [:asc "a"]})
               [:t {:a "a"} :order-by [:a :asc]])))
     (t "with one filter and two sorts"
        (is (= (carte-table-adapter :t {:a "a"} {:sort [:asc "a" :desc "b"]})
             [:t {:a "a"} :order-by [:a :asc] [:b :desc]])))))

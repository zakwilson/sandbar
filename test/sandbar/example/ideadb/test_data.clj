;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.example.ideadb.test-data
  (:use (clojure test)
        (sandbar [test :only (t)])
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

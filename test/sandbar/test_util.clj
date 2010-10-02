;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.test-util
  (:use (clojure test)
        (sandbar util
                 [test :only (t)])))
 
(deftest test-remove-file-ext
  (is (= (remove-file-ext "test.txt") "test"))
  (is (= (remove-file-ext "test.file.txt") "test.file"))
  (is (= (remove-file-ext "test") "test")))

(deftest test-index-by
  (t "index by"
     (t "when you have two distinct maps in a collection"
        (is (= (index-by :a [{:a "1" :b "A"}
                             {:a "2" :b "B"}])
               {"1" {:a "1" :b "A"}
                "2" {:a "2" :b "B"}})))))

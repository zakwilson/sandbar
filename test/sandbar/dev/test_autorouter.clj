;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.dev.test-autorouter
  (:use (clojure test)
	    (sandbar.dev autorouter)))
	
(deftest test-uri-seq
	(is (= (uri-seq {:uri nil})
		[]))
	(is (= (uri-seq {:uri ""})
		[]))
	(is (= (uri-seq {:uri "/"})
		[]))
	(is (= (uri-seq {:uri "/hello"})
		["hello"]))
	(is (= (uri-seq {:uri "/hello/world"})
		["hello" "world"]))
	(is (= (uri-seq {:uri "/admin/hello/world"} ["/admin"])
		["hello" "world"]))
	(is (= (uri-seq {:uri "/admin/hello/world"} ["/user" "/admin"])
		["hello" "world"]))
	(is (= (uri-seq {:uri "/admin/user/list"} ["/user" "/admin"])
		["user" "list"])))
;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.core-test
  (:use (clojure test)
        (sandbar core test)))

;;
;; Working with context paths
;;

(deftest set-app-context!-test
  (binding [app-context (atom "")]
    (let [result (set-app-context! "/a")])
    (is (= @app-context "/a"))))

(deftest cpath-test
  (binding [app-context (atom "/a")]
    (is (= (cpath "/b")
           "/a/b"))
    (is (= (cpath "b")
           "b")))
  (binding [app-context (atom "")]
    (is (= (cpath "/b")
           "/b"))
    (is (= (cpath "b")
           "b"))))

(deftest remove-cpath-test
  (binding [app-context (atom "/a")]
    (is (= (remove-cpath "/a/b")
           "/b"))
    (is (= (remove-cpath "/b")
           "/b"))
    (is (= (remove-cpath "b")
           "b")))
  (binding [app-context (atom "")]
    (is (= (remove-cpath "/b")
           "/b"))
    (is (= (remove-cpath "b")
           "b"))))

(deftest clink-to-test
  (binding [app-context (atom "/a")]
    (is (= (clink-to "/b" "b")
           [:a {:href "/a/b"} ["b"]]))))

;;
;; Resource location
;;

(deftest set-resource-url-prefix!-test
  (binding [resource-url-prefix (atom "")]
    (let [result (set-resource-url-prefix! "/a")])
    (is (= @resource-url-prefix)
        "/a")))

(deftest set-resource-url-prefix!-test
  (binding [app-context (atom "")
            resource-url-prefix (atom "")]
    (is (= (resource-path "/b")
           "/b"))
    (is (= (resource-path "b")
           "b"))
    (is (= (css-path) "/css/"))
    (is (= (image-path) "/images/"))
    (is (= (js-path) "/js/")))
  (binding [app-context (atom "/a")
            resource-url-prefix (atom "")]
    (is (= (resource-path "/b")
           "/a/b"))
    (is (= (resource-path "b")
           "b"))
    (is (= (css-path) "/a/css/"))
    (is (= (image-path) "/a/images/"))
    (is (= (js-path) "/a/js/")))
  (binding [app-context (atom "/a")
            resource-url-prefix (atom "/c")]
    (is (= (resource-path "/b")
           "/c/b"))
    (is (= (resource-path "b")
           "b"))))

;;
;; Redirects
;;

(deftest append-to-redirect-loc-test
  (t "append to redirect location"
     (binding [app-context (atom "")]
       (t "when append is blank"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "")
                 (redirect-301 "/p"))))
       (t "when append is nil"
          (is (= (append-to-redirect-loc (redirect-301 "/p") nil)
                 (redirect-301 "/p"))))
       (t "when there is something to append"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "/t")
                 (redirect-301 "/t/p"))))
       (t "does nothing when loc is a complete URL with http scheme"
          (is (= (append-to-redirect-loc (redirect-301 "http://x/p") "/t")
                 (redirect-301 "http://x/p"))))
       (t "does nothing when loc is a complete URL with https scheme"
          (is (= (append-to-redirect-loc (redirect-301 "https://x/p") "/t")
                 (redirect-301 "https://x/p")))))
     (binding [app-context (atom "/context")]
       (t "when append is blank and there is a context"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "")
                 (redirect-301 "/p"))))
       (t "when there is something to append and there is a context"
          (is (= (append-to-redirect-loc (redirect-301 "/p") "/t")
                 (redirect-301 "/t/p")))))))

;;
;; HTML Page Helpers
;;

(deftest link-to-js-test
  (t "link-to-js"
     (t "with two arguments"
        (is (= (link-to-js (f "x") "y")
               [:a {:href "javascript:f('x');"} ["y"]])))
     (t "with three arguments"
        (is (= (link-to-js (f "x") "y" :i)
               [:a {:href "javascript:f_i('x');"} ["y"]])))
     (t "with hyphen in qualifier"
        (is (= (link-to-js (f "x") "y" :i-m)
               [:a {:href "javascript:f_i_m('x');"} ["y"]])))))



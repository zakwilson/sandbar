;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.core-test
  (:use (clojure test)
        (hiccup page-helpers)
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
  (binding [resource-url-prefix (atom "/b")]
    (is (= @resource-url-prefix "/b"))
    (is (do (set-resource-url-prefix! "/a")
            (= @resource-url-prefix "/a"))))
  (is (= @resource-url-prefix "")))

(deftest resource-path-test
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

(deftest redirect?-test
  (is (true? (redirect? (redirect-301 "/a"))))
  (is (true? (redirect? {:status 302
                        :headers {"Location" "/a"}})))
  (is (false? (redirect? {:status 200}))))

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
;; Utilities
;;

(deftest property-lookup-test
  (is (= (property-lookup {} :a)
         "a"))
  (is (= (property-lookup {:a "b"} :a)
         "b")))

(deftest get-param-test
  (let [params {"a" "a" :b "b" "c" "1" :d "2" :e 3}
        tfn (partial get-param params)]
    (is (= (tfn :a) "a"))
    (is (= (tfn :b) "b"))
    (is (= (tfn :c) 1))
    (is (= (tfn :d) 2))
    (is (= (tfn :e) 3))))

;;
;; HTML Page Helpers
;;

(deftest stylesheet-test
  (is (= (stylesheet "a.css")
         (include-css "/css/a.css"))))

(deftest javascript-test
  (is (= (javascript "a.js")
         (include-js "/js/a.js"))))

(deftest icon-test
  (is (= (icon "i.png")
         [:link {:rel "icon" :type "image/png" :href "/images/i.png"}]))
  (is (= (icon "i.ico")
         [:link {:rel "icon" :type "image/vnd.microsoft.icon"
                 :href "/images/i.ico"}]))
  (is (= (icon "image/x-icon" "i.ico")
         [:link {:rel "icon" :type "image/x-icon"
                 :href "/images/i.ico"}])))

(deftest image-test
  (is (= (image "a.png")
         [:img {:src "/images/a.png" :alt "a.png" :border "0"}]))
  (is (= (image "a.png" {:border "2" :alt "A"})
         [:img {:src "/images/a.png" :alt "A" :border "2"}]))
  (is (= (image "a.png" "b.png")
         [:img {:src "/images/a.png"
                :alt "a.png"
                :border "0" 
                :onmouseout "this.src='/images/a.png'" 
                :onmouseover "this.src='/images/b.png'"}]))
  (is (= (image "a.png" "b.png" {:alt "A"})
         [:img {:src "/images/a.png"
                :alt "A"
                :border "0" 
                :onmouseout "this.src='/images/a.png'" 
                :onmouseover "this.src='/images/b.png'"}])))

(deftest image-link-test
  (is (= (image-link "/a" "a.png")
         (link-to "/a" (image "a.png"))))
  (is (= (image-link "/a" "a.png" {:alt "A"})
         (link-to "/a" (image "a.png" {:alt "A"}))))
  (is (= (image-link "/a" "a.png" "b.png")
         (link-to "/a" (image "a.png" "b.png"))))
  (is (= (image-link "/a" "a.png" "b.png" {:alt "A"})
         (link-to "/a" (image "a.png" "b.png" {:alt "A"})))))

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



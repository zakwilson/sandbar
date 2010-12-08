;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.core
  "Common code used throughout Sandbar."
  (:use [hiccup.page-helpers :only [link-to include-css include-js]]))

;;
;; Working with context paths
;;

(def app-context (atom ""))

(defn set-app-context!
  "Set the context path for this appliation. The context path will be used by
   cpath and clink-to so that you don't have to repeat it throughout the
   application."
  [context]
  (swap! app-context (constantly context)))

(defn cpath
  "Prefix a path starting with '/' with the context path."
  [path]
  (if (.startsWith path "/")
    (str @app-context path)
    path))

(defn remove-cpath
  "Strip the context path from a path."
  [path]
  (let [c @app-context]
    (if (and (not (empty? c))
             (.startsWith path c))
      (apply str (drop (count c) path))
      path)))

(defn clink-to
  "Create a link to path, prefixing the path with the context path."
  [path title]
  (link-to (cpath path) title))

;;
;; Resource location
;;

(def resource-url-prefix (atom ""))

(defn set-resource-url-prefix!
  "Set the value of the resource-url-prefix. If the resource-url-prefix is the
   default value then resources are assumed to be under css and js directores.
   If resources are located in a CDN or under some other base URL then that
   can be set here."
  [prefix]
  (swap! resource-url-prefix (constantly prefix)))

(defn resource-path
  "Get the root path for application resources. If the resource-url-prefix is
   set, then this prefix will be used, otherwise delegate to cpath."
  [s]
  (cond (not (.startsWith s "/")) s
        (empty? @resource-url-prefix) (cpath s)
        :else (str @resource-url-prefix s)))

(defn css-path []
  (resource-path "/css/"))
(defn image-path []
  (resource-path "/images/"))
(defn js-path []
  (resource-path "/js/"))

;;
;; Redirects
;;

(defn redirect-301 [url]
  {:status 301
   :headers {"Location" (cpath url)}})

(defn redirect? [m]
  (or (= (:status m) 302)
      (= (:status m) 301)))

(defn append-to-redirect-loc
  "Append the uri-prefix to the value of Location in the headers of the
   redirect map."
  [m uri-prefix]
  (if (or (nil? uri-prefix) (empty? uri-prefix))
    m
    (let [loc (remove-cpath ((:headers m) "Location"))]
      (if (re-matches #".*://.*" loc)
        m
        (merge m {:headers {"Location" (cpath (str uri-prefix loc))}})))))

;;
;; Utilities
;;

(defn property-lookup
  "Return the value of the key in the passed map or the name of the key."
  [p k]
  (k p (name k)))

(defn- filter-param-value [v]
  (try (Integer/parseInt v)
       (catch Exception _ (try (BigDecimal. v)
                               (catch Exception _ v)))))

(defn get-param
  "Get a parameter from the params map where the key may be a string or
   a keyword. Automatically coerce numbers."
  [params key]
  (let [p (or (get params key) (get params (name key)))]
    (cond (string? p) (filter-param-value p)
          (coll? p) (map filter-param-value p)
          :else p)))

;;
;; HTML Page Helpers
;;

(defn stylesheet
  "Convert a css file name into the correct path to that file."
  [name]
  (include-css (str (css-path) name)))

(defn javascript
  "Convert a javascript file name into the correct path to that file."
  [name]
  (include-js (str (js-path) name)))

(defn icon
  "Create a link to an icon with the correct mime type."
  ([name]
     (let [type (last (seq (.split name "[.]")))
           type (str "image/"
                     (if (= type "ico")
                       "vnd.microsoft.icon"
                       type))]
       (icon type name)))
  ([type name]
     [:link {:rel "icon" :type type :href (str (image-path) name)}]))

(defn image
  "Create an image element or an image with a mouseover."
  ([name] (image name {}))
  ([name & options]
     (let [attrs (first (filter map? options))
           mouseover (first (filter string? options))
           image-path (image-path)
           attrs (merge {:src (str image-path name)
                         :border "0"
                         :alt name}
                        attrs)
           attrs
           (if mouseover
             (merge attrs
                    {:onmouseout (str "this.src='" image-path name "'") 
                     :onmouseover (str "this.src='" image-path mouseover "'")})
             attrs)]
       [:img attrs])))

(defn image-link
  "Create an image link element.
   (image-link x y z) is the same as (clink-to x (image y z))."
  ([path name] (image-link path name {:alt name})) 
  ([path name & options]
    (clink-to path (apply image name options))))

(defmacro link-to-js
  "Create a link that will call a javascript function."
  [& args]
  (let [[form title qualifier] args
        function (str (name (first form)))
        args (rest form)]
    `(link-to
      (str "javascript:"
           ~function
           (when ~qualifier
             (str "_" (.replaceAll (name ~qualifier) "-" "_")))
           "("
           (apply str
                  (interpose ", "
                             (map (fn [a#] (if (string? a#)
                                             (str "'" a# "'")
                                             a#))
                                  [~@args])))
           ");") ~title)))


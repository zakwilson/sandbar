(defproject sandbar/sandbar "0.3.0-SNAPSHOT"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :dependencies [[org.clojure/clojure "1.2.0-RC1"]
                 [org.clojure/clojure-contrib "1.2.0-RC1"]
                 [compojure "0.4.0"]
                 [hiccup "0.2.6"]
                 [inflections "0.4-SNAPSHOT"]]
  :dev-dependencies [[jline "0.9.94"]
                     [ring/ring-devel "0.2.3"]
                     [ring/ring-jetty-adapter "0.2.3"]
                     [ring/ring-httpcore-adapter "0.2.3"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [carte/carte "0.1.2"]
                     [deview/lein-deview "1.0.5"]]
  :deview-server 9001
  :deview-loc-ext #{"clj" "js" "css" "xml"})

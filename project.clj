(defproject sandbar/sandbar "0.3.0-SNAPSHOT"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [compojure "0.4.0"]
                 [hiccup "0.2.6"]
                 [inflections "0.3"]]
  :dev-dependencies [[lein-clojars "0.5.0-SNAPSHOT"]
                     [jline "0.9.94"]
                     [ring/ring-devel "0.2.3"]
                     [ring/ring-jetty-adapter "0.2.3"]
                     [ring/ring-httpcore-adapter "0.2.3"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [carte/carte "0.1.2"]
                     [deview/lein-deview "1.0.1"]]
  :namespaces [sandbar.test
               sandbar.util
               sandbar.core
               sandbar.stateful-session
               sandbar.auth
               sandbar.dev.autorouter
               sandbar.dev.basic-authentication
               sandbar.dev.forms
               sandbar.dev.list-manager
               sandbar.dev.standard-pages
               sandbar.dev.tables
               sandbar.dev.stats
               sandbar.dev.user-manager
               sandbar.dev.validation]
  :deview-server 9001)

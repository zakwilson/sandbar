(defproject sandbar/examples "0.2.4"
  :description "Example code for the sandbar project. Includes the ideadb
                reference application."
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [compojure "0.4.0-RC3"]
                 [hiccup "0.2.6"]
                 [ring/ring-jetty-adapter "0.2.3"]
                 [sandbar/sandbar-core "0.3.1"]
                 [sandbar/sandbar-session "0.2.4"]
                 [sandbar/sandbar-auth "0.2.3"]
                 [sandbar/sandbar-dev "0.0.1-SNAPSHOT"]
                 [carte/carte "0.1.0"]]
  :dev-dependencies [[ring/ring-devel "0.2.3"]
                     [ring/ring-httpcore-adapter "0.2.3"]
                     [ring/ring-servlet "0.2.3"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [jline "0.9.94"]])

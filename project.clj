(defproject sandbar/sandbar "0.3.0"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :url "http://github.com/brentonashworth/sandbar"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [ring/ring-core "0.3.1"]
                 [compojure "0.5.1"]
                 [hiccup "0.3.0"]
                 [inflections "0.4-SNAPSHOT"]]
  :dev-dependencies [[jline "0.9.94"]
                     [ring/ring-devel "0.3.1"]
                     [ring/ring-jetty-adapter "0.3.1"]
                     [ring/ring-httpcore-adapter "0.3.1"]
                     [mysql/mysql-connector-java "5.1.6"]
                     [carte/carte "0.2.0-SNAPSHOT"]
                     [deview/lein-deview "1.2.0-SNAPSHOT"]
                     [lein-difftest "1.3.1"]
                     [enlive "1.0.0-SNAPSHOT"]
                     [radagast "1.0.0"]]
  :hooks [leiningen.hooks.difftest]
  :namespaces [sandbar.dev.tables]
  :deview-server 9001
  :deview-loc-ext #{"clj" "js" "css" "xml"}
  :radagast/ns-whitelist #"^sandbar.*")

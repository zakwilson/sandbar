(defproject sandbar/sandbar "0.4.0-SNAPSHOT"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :url "http://github.com/brentonashworth/sandbar"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [ring/ring-core "0.3.5"]
                 [compojure "0.5.3"]
                 [hiccup "0.3.1"]
                 [inflections "0.4-SNAPSHOT"]]
  :dev-dependencies [[jline "0.9.94"]
                     [ring/ring-devel "0.3.5"]
                     [ring/ring-jetty-adapter "0.3.5"]
                     [ring/ring-httpcore-adapter "0.3.5"]
                     [lein-difftest "1.3.2-SNAPSHOT"]
                     [radagast "1.0.0"]
                     [enlive "1.0.0-SNAPSHOT"]]
  :hooks [leiningen.hooks.difftest]
  :namespaces [sandbar.tables]
  :radagast/ns-whitelist #"^sandbar.*")

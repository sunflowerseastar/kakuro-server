(defproject kakuro-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[clojail "1.0.6"]
                 [compojure "1.6.1"]
                 [criterium "0.4.5"]
                 [incanter "1.9.3"]
                 [metosin/muuntaja "0.6.7"]
                 [metosin/ring-http-response "0.9.1"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.logic "1.0.0"]
                 [ring "1.8.1"]
                 [ring-cors "0.1.13"]
                 [tupelo "0.9.175"]]
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler kakuro-server.core/api
         :port 3001}
  :repl-options {:init-ns kakuro-server.core})

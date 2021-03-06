(defproject clojure-perf "0.1.0-SNAPSHOT"
  :description "A simple harness for evaluating the performance of different approaches to common operations in Clojure."
  :url "https://github.com/matgis/clojure-perf"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [criterium "0.4.4"]]
  :main clojure-perf.main)

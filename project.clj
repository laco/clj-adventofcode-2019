(defproject clj-adventofcode-2019 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/core.async "0.6.532"]
                 ]
  :main ^:skip-aot clj-adventofcode-2019.day13
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})

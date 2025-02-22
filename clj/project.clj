(defproject aoc-2024 "0.1.0-SNAPSHOT"
  :description "Advent of Code, in Clojure (2024)"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [dev.weavejester/medley "1.8.1"]
                 [criterium "0.4.6"]
                 [org.clojure/math.combinatorics "0.3.0"]]
  :main ^:skip-aot aoc-2024.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})

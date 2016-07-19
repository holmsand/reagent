(defproject reagent "0.6.0-SNAPSHOT"
  :url "http://github.com/reagent-project/reagent"
  :license {:name "MIT"}
  :description "A simple ClojureScript interface to React"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.89"]
                 [cljsjs/react-dom "15.2.1-0"]
                 [cljsjs/react-dom-server "15.2.1-0"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [codox "0.9.0"]]

  :hooks [leiningen.cljsbuild]

  :source-paths ["src"]

  :codox {:language :clojurescript
          :exclude clojure.string}

  :profiles {:test {:cljsbuild
                    {:builds {:client {:source-paths ["test"]
                                       :compiler
                                       {:main "reagenttest.runtests"}}}}}

             :fig [{:dependencies [[figwheel "0.5.4-7"]]
                    :plugins [[lein-figwheel "0.5.4-7"]]
                    :source-paths ["demo"] ;; for lighttable
                    :resource-paths ["site" "outsite"]
                    :figwheel {:css-dirs ["site/public/css"]}
                    :cljsbuild
                    {:builds
                     {:client
                      {:figwheel true
                       :compiler {:optimizations :none
                                  :recompile-dependents false
                                  :output-dir "outsite/public/js/out"
                                  :asset-path "js/out"}}}}}]

             :site {:resource-paths ^:replace ["outsite"]
                    :figwheel {:css-dirs ^:replace ["outsite/public/css"]}
                    :cljsbuild
                    {:builds {:client
                              {:notify-command ["node" "bin/gen-site.js"]}}}}

             :advanced {:cljsbuild
                        {:builds {:client
                                  {:compiler {:optimizations :advanced
                                              :elide-asserts true
                                              :pretty-print false
                                              ;; :pseudo-names true
                                              :output-dir "target/client"}}}}}

             :prod [:advanced :site]

             :webpack {:cljsbuild
                       {:builds {:client
                                 {:compiler
                                  {:foreign-libs
                                   [{:file "target/webpack/bundle.js"
                                     :file-min "target/webpack/bundle.min.js"
                                     :provides ["cljsjs.react.dom"
                                                "cljsjs.react.dom.server"
                                                "cljsjs.react"]
                                     :requires []}]}}}}}

             :prod-test [:prod :test]

             :dev [:fig :test]

             :dev-notest [:fig]}

  :clean-targets ^{:protect false} [:target-path :compile-path
                                    "outsite/public/js"
                                    "outsite/public/site"
                                    "outsite/public/news"
                                    "outsite/public/css"
                                    "outsite/public/index.html"
                                    "out"]

  :cljsbuild {:builds {:client
                       {:source-paths ["src"
                                       "demo"
                                       "examples/todomvc/src"
                                       "examples/simple/src"
                                       "examples/geometry/src"]
                        :compiler {:parallel-build true
                                   :main "reagentdemo.core"
                                   :output-to "outsite/public/js/main.js"}}}
              :test-commands {"test-phantom" ["phantomjs" "bin/phantom.js"]
                              "test-node" ["node" "bin/gen-site.js"]}}

  :figwheel {:http-server-root "public" ;; assumes "resources"
             :repl false})

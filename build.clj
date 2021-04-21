(require '[cljs.build.api :as b])

(b/build "src/pf"
         {
          :main 'pf.core
          :output-to "pf.js"
          :warnings true
          :optimizations :simple
          :source-map false
          ;:source-map true
          :source-paths ["src/pf"]
          ;:language-in :es-next
          ;:install-deps true
          :npm-deps {
                     }})

(defproject t-digest "0.1.0-SNAPSHOT"
  :description "A Clojure(Script) library for on-line accumulation of rank-based statistics using the t-digest"
  :url "https://github.com/henrygarner/t-digest"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/test.check "0.9.0"]
                                  [com.tdunning/t-digest "3.1"]]
                   ;; :java-source-paths ["java/t-digest/src/main/java"]
                   :plugins [[lein-auto "0.1.3"]]}})

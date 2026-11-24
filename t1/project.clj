(defproject tvdsl-t1 "0.1.0-SNAPSHOT"
  :description "Clojure solution for TVDSL T1: runtime ImmutableIntArray and codegen prototype"
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [org.ow2.asm/asm "9.9.1"]]
  :source-paths ["src" "../t0/src" "../hw4/src" "../hw3/src" "../hw2/src" "../hw1/src"]
  :java-source-paths ["java"]
  :javac-options ["--release" "21"]
  :test-paths ["test"])

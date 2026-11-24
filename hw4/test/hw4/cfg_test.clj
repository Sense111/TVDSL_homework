(ns hw4.cfg-test
  (:require [clojure.test :refer [deftest is]]
            [hw3.types :as types]
            [hw4.cfg :as cfg]
            [hw4.interp :as interp]
            [hw4.ir :as ir]))

(defn- fn-ir [src]
  (first (ir/gen-module (:ast (types/check-source src)))))

(def loop-src
  "f(a: int[], n: int) returns r: int uses i { r = 0; i = 0; while (i < n) { r = r + a[i]; i = i + 1; } }")

(deftest while-loop-has-a-back-edge
  (let [g (cfg/build (:code (fn-ir loop-src)))
        p (cfg/preds g)]
    (is (= 4 (count (:order g))))
    (is (some #(> (count (get p %)) 1) (:order g)) "заголовок цикла имеет два предшественника")
    (is (every? #(ir/terminator? (peek (:instrs (get-in g [:blocks %])))) (:order g)))))

(deftest every-block-ends-with-a-terminator-and-has-matching-succs
  (let [g (cfg/build (:code (fn-ir "f(x: int) returns r: int { if (x > 0 or x < -5) r = 1; else r = 2; }")))]
    (doseq [id (:order g)
            :let [b (get-in g [:blocks id])]]
      (is (ir/terminator? (peek (:instrs b))))
      (is (every? (set (:order g)) (:succs b))))))

(deftest unreachable-blocks-are-removed
  (let [code [[:jmp "L1"] [:label "L2"] [:mov "x" 1] [:jmp "L1"] [:label "L1"] [:ret]]
        g (cfg/build code)]
    (is (= #{"B0" "L1"} (cfg/reachable g)))
    (is (= ["B0" "L1"] (:order (cfg/remove-unreachable g))))))

(deftest linearize-round-trips-the-behaviour
  (let [f (fn-ir loop-src)
        g (cfg/build (:code f))
        f2 (assoc f :code (cfg/linearize g))]
    (doseq [args [[[1 2 3] 3] [[] 0] [[5] 3]]]
      (is (= (interp/outcome [f] "f" args) (interp/outcome [f2] "f" args))))))

(deftest liveness-of-loop-variables
  (let [f (fn-ir loop-src)
        g (cfg/build (:code f))
        lv (cfg/liveness g (:returns f))]
    (is (= #{"a" "n"} (get-in lv [(:entry g) :in])))
    (is (= #{"r"} (get-in lv [(last (:order g)) :in])))
    (is (contains? (get-in lv ["L1" :in]) "i"))))

(deftest live-after-per-instruction
  (let [instrs [[:mov "t" 1] [:bin :add "u" "t" 2] [:ret]]
        after (cfg/live-after instrs #{} ["u"])]
    (is (= [#{"t"} #{"u"} #{}] after))))

(deftest dot-and-text-output
  (let [f (fn-ir loop-src) g (cfg/build (:code f))]
    (is (re-find #"digraph" (cfg/->dot "f" g)))
    (is (re-find #"->" (cfg/->dot "f" g)))
    (is (re-find #"return" (cfg/->text g)))))

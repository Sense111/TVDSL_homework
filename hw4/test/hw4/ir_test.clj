(ns hw4.ir-test
  (:require [clojure.test :refer [deftest is]]
            [hw3.types :as types]
            [hw4.ir :as ir]))

(defn- gen [src & [opts]]
  (let [{:keys [ast errors]} (types/check-source src)]
    (assert (empty? errors) (pr-str errors))
    (ir/gen-module ast (or opts {}))))

(deftest arithmetic-uses-temporaries-and-writes-result-directly
  (let [f (first (gen "f(x: int, y: int) returns r: int { r = x + y * 2; }"))]
    (is (= [[:bin :mul "%t1" "y" 2]
            [:bin :add "r" "x" "%t1"]
            [:ret]]
           (:code f)))
    (is (= {"x" :int "y" :int "r" :int "%t1" :int} (:var-types f)))))

(deftest constants-are-folded-by-the-generator-only-for-literals
  (is (= [[:mov "r" -5] [:ret]] (:code (first (gen "f() returns r: int { r = -5; }")))))
  (is (= [[:mov "r" -2147483648] [:ret]]
         (:code (first (gen "f() returns r: int { r = -2147483648; }"))))))

(deftest if-else-uses-lazy-conditions
  (let [code (:code (first (gen "f(x: int, y: int) returns r: int { if (x > 0 and y > 0) r = 1; else r = 2; }")))
        jumps (filter #(= :cjmp (first %)) code)]
    (is (= 2 (count jumps)))
    (is (= [:gt "x" 0] (vec (take 3 (rest (first jumps))))))))

(deftest array-update-is-aset-and-length-is-len
  (let [code (:code (first (gen "f(a: int[]) returns b: int[] uses n { b = a; b[0] = 7; n = length(b); }")))]
    (is (= [:mov "b" "a"] (first code)))
    (is (= [:aset "b" "b" 0 7] (second code)))
    (is (= [:len "n" "b"] (nth code 2)))))

(deftest tuple-call-becomes-call-with-several-destinations
  (let [m (gen "g() returns a: int, b: int { a = 1; b = 2; }\nf() returns r: int uses x, y { x, y = g(); r = x + y; }")]
    (is (= [:call ["x" "y"] "g" []] (first (:code (second m)))))))

(deftest asserts-are-ignored-unless-requested
  (let [src "f(x: int) returns r: int { assert x > 0; r = x; }"]
    (is (not-any? #(= :trap (first %)) (:code (first (gen src)))))
    (is (some #(= :trap (first %)) (:code (first (gen src {:asserts true})))))))

(deftest wrap32-wraps-around
  (is (= -2147483648 (ir/wrap32 2147483648)))
  (is (= 2147483647 (ir/wrap32 -2147483649)))
  (is (= 5 (ir/wrap32 5))))

(deftest defs-and-uses
  (is (= ["d"] (ir/instr-defs [:bin :add "d" "a" 1])))
  (is (= ["a"] (ir/instr-uses [:bin :add "d" "a" 1])))
  (is (= ["x" "y"] (ir/instr-defs [:call ["x" "y"] "g" []])))
  (is (= ["p" "q"] (ir/instr-uses [:cjmp :lt "p" "q" "L1" "L2"])))
  (is (true? (ir/may-trap? [:bin :div "d" "a" "b"])))
  (is (false? (ir/may-trap? [:bin :add "d" "a" "b"]))))

(deftest text-output-is-readable
  (is (= "  %t1 = x + 1" (ir/instr->text [:bin :add "%t1" "x" 1])))
  (is (= "  if a < b goto L1 else L2" (ir/instr->text [:cjmp :lt "a" "b" "L1" "L2"]))))

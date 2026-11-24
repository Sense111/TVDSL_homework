(ns hw4.interp-test
  (:require [clojure.test :refer [deftest is]]
            [hw3.types :as types]
            [hw4.interp :as interp]
            [hw4.ir :as ir]))

(defn- module [src] (ir/gen-module (:ast (types/check-source src))))

(deftest runs-loops-arrays-and-tuples
  (let [m (module (str "sum(a: int[], n: int) returns r: int uses i { r = 0; i = 0; while (i < n) { r = r + a[i]; i = i + 1; } }\n"
                       "divmod(x: int, y: int) returns q: int, r: int { q = x / y; r = x - q * y; }"))]
    (is (= {:ok [10]} (interp/outcome m "sum" [[1 2 3 4] 4])))
    (is (= {:ok [3 1]} (interp/outcome m "divmod" [10 3])))
    (is (= {:ok [-3 -1]} (interp/outcome m "divmod" [-10 3])) "деление усекается к нулю")))

(deftest runtime-errors-are-reported
  (let [m (module (str "d(x: int) returns r: int { r = 10 / x; }\n"
                       "g(a: int[], i: int) returns r: int { r = a[i]; }"))]
    (is (= {:error :div-zero} (interp/outcome m "d" [0])))
    (is (= {:error :index} (interp/outcome m "g" [[1 2] 2])))
    (is (= {:error :index} (interp/outcome m "g" [[1 2] -1])))))

(deftest int32-wraparound
  (let [m (module "f(x: int) returns r: int { r = x + 1; }\ng(x: int) returns r: int { r = x * x; }")]
    (is (= {:ok [-2147483648]} (interp/outcome m "f" [2147483647])))
    (is (= {:ok [0]} (interp/outcome m "g" [65536])))))

(deftest recursion-and-array-update
  (let [m (module (str "fact(n: int) returns r: int uses t { if (n <= 1) r = 1; else { t = fact(n - 1); r = n * t; } }\n"
                       "set1(a: int[]) returns b: int[] { b = a; b[0] = 9; }"))]
    (is (= {:ok [120]} (interp/outcome m "fact" [5])))
    (is (= {:ok [[9 2]]} (interp/outcome m "set1" [[1 2]])))))

(deftest asserts-trap-only-when-compiled-in
  (let [src "f(x: int) returns r: int { assert x > 0; r = x; }"
        plain (ir/gen-module (:ast (types/check-source src)))
        checked (ir/gen-module (:ast (types/check-source src)) {:asserts true})]
    (is (= {:ok [-1]} (interp/outcome plain "f" [-1])))
    (is (= {:error :assert} (interp/outcome checked "f" [-1])))
    (is (= {:ok [2]} (interp/outcome checked "f" [2])))))

(deftest infinite-loop-hits-the-step-limit
  (let [m (module "f() returns r: int { while (1 > 0) r = r + 1; }")]
    (is (= {:error :fuel} (interp/outcome m "f" [])))))

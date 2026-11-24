(ns t1.semantic-test
  "Тесты анализа «массив определённо присвоен» (t1.semantic): ветки if, циклы while, вложенность."
  (:require [clojure.test :refer [deftest is testing]]
            [t1.compile :as c]))

(defn- errors [src] (:errors (c/compile-source src)))
(defn- ok? [src] (nil? (:errors (c/compile-source src))))
(defn- uninit? [src] (= :uninitialized-array (:kind (first (errors src)))))

(deftest if-else-both-branches
  (testing "присвоено в обеих ветках — запись после if проходит"
    (is (ok? "function f(s: int[], c: int) returns a: int[] {
                if (c > 0) a = s; else a = s; a[0] = 1; }")))
  (testing "присвоено в блоках обеих ветвей"
    (is (ok? "function f(s: int[], c: int) returns a: int[] uses t {
                if (c > 0) { t = 1; a = s; } else { a = s; t = 2; } a[0] = t; }"))))

(deftest if-without-else
  (testing "присвоено только в then — после if запись запрещена"
    (is (uninit? "function f(s: int[], c: int) returns a: int[] {
                    if (c > 0) a = s; a[0] = 1; }")))
  (testing "присвоено до if — остаётся присвоенным после"
    (is (ok? "function f(s: int[], c: int) returns a: int[] {
                a = s; if (c > 0) a[0] = 1; a[1] = 2; }")))
  (testing "запись внутри ветки после присваивания в этой же ветке"
    (is (ok? "function f(s: int[], c: int) returns a: int[] {
                if (c > 0) { a = s; a[0] = 1; } }"))))

(deftest if-else-only-one-branch
  (is (uninit? "function f(s: int[], c: int) returns a: int[] uses d {
                  if (c > 0) a = s; else d = 1; a[0] = 1; }")))

(deftest while-loops
  (testing "присвоено до while — запись в теле проходит и после цикла тоже"
    (is (ok? "function f(s: int[], n: int) returns a: int[] uses i {
                a = s; i = 0; while (i < n) { a[i] = i; i = i + 1; } a[0] = 5; }")))
  (testing "присвоено только внутри while — после цикла ошибка"
    (is (uninit? "function f(s: int[], n: int) returns a: int[] uses c {
                    c = n; while (c > 0) { a = s; c = c - 1; } a[0] = 1; }")))
  (testing "присвоение в теле не действует на начало тела (первая итерация)"
    (is (uninit? "function f(s: int[], n: int) returns a: int[] uses c {
                    c = n; while (c > 0) { a[0] = 1; a = s; c = c - 1; } }")))
  (testing "присвоение и запись внутри тела подряд"
    (is (ok? "function f(s: int[], n: int) returns a: int[] uses c {
                c = n; while (c > 0) { a = s; a[0] = 1; c = c - 1; } }"))))

(deftest nested-flow
  (testing "if внутри while, присвоение в обеих ветках"
    (is (ok? "function f(s: int[], n: int) returns a: int[] uses c {
                c = n; while (c > 0) { if (c > 5) a = s; else a = s; a[0] = c; c = c - 1; } }")))
  (testing "while внутри ветки if не даёт присвоенности после if"
    (is (uninit? "function f(s: int[], n: int) returns a: int[] uses c {
                    c = n; if (c > 0) { while (c > 0) { a = s; c = c - 1; } } else a = s; a[0] = 1; }"))))

(deftest copy-and-call
  (testing "a = b: b не присвоен — a не присвоен"
    (is (uninit? "function f() returns a: int[], b: int[] { a = b; a[0] = 1; }")))
  (testing "a = b: b — параметр"
    (is (ok? "function f(b: int[]) returns a: int[] { a = b; a[0] = 1; }")))
  (testing "массив из вызова функции считается присвоенным"
    (is (ok? "function mk(b: int[]) returns a: int[] { a = b; }
              function g(b: int[]) returns a: int[] { a = mk(b); a[0] = 1; }"))))

(deftest error-shape
  (let [e (first (errors "function f(c: int) returns a: int[] {\n  if (c > 0) a[0] = 1;\n}"))]
    (is (= :semantic (:phase e)))
    (is (= :uninitialized-array (:kind e)))
    (is (= [2 14] [(:line e) (:column e)]))
    (is (string? (:message e)))))

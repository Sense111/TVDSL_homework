(ns t1.runtime-test
  "Тесты JVM-класса t1.ImmutableIntArray (java/t1/ImmutableIntArray.java)."
  (:require [clojure.test :refer [deftest is testing]])
  (:import (t1 ImmutableIntArray)))

(defn- arr ^ImmutableIntArray [& xs] (ImmutableIntArray/of (int-array xs)))

(deftest factories-and-length
  (is (= 3 (.length (arr 1 2 3))))
  (is (= 0 (.length (arr))))
  (let [src (int-array [4 5])
        a (ImmutableIntArray/copyOf src)]
    (aset src 0 99)
    (is (= 4 (.get a 0)) "copyOf не делит память с исходным"))
  (let [src (int-array [4 5])
        a (ImmutableIntArray/of src)]
    (aset src 0 99)
    (is (= 4 (.get a 0)) "of тоже копирует")))

(deftest get-checks-bounds
  (let [a (arr 10 20)]
    (is (= 20 (.get a 1)))
    (is (thrown? ArrayIndexOutOfBoundsException (.get a 2)))
    (is (thrown? ArrayIndexOutOfBoundsException (.get a -1)))))

(deftest with-is-functional
  (let [a (arr 1 2 3)
        b (.with a 1 42)]
    (testing "новый массив с изменённым элементом"
      (is (= 42 (.get b 1)))
      (is (not (identical? a b))))
    (testing "исходный массив не меняется"
      (is (= 2 (.get a 1))))
    (testing "неверный индекс в with бросает"
      (is (thrown? ArrayIndexOutOfBoundsException (.with a 3 0))))))

(ns t1.codegen-test
  "Тесты T1: Funny -> байт-код -> загрузка -> вызов рефлексией.
  Деление на 0 -> ArithmeticException, выход за границы -> ArrayIndexOutOfBoundsException (исключения JVM,
  явных проверок в сгенерированном коде нет)."
  (:require [clojure.test :refer [deftest is testing]]
            [t1.compile :as c])
  (:import (java.lang.reflect InvocationTargetException Modifier)
           (t1 ImmutableIntArray)))

(defn- load-class [source]
  (let [r (c/load-module source)]
    (is (nil? (:errors r)) (pr-str (:errors r)))
    (:class r)))

(defn- call
  "Вызывает public static метод `name`; исключение из метода разворачивается из InvocationTargetException."
  [^Class cls name & args]
  (let [m (first (filter #(= name (.getName ^java.lang.reflect.Method %)) (.getMethods cls)))]
    (try (.invoke m nil (object-array args))
         (catch InvocationTargetException e (throw (.getCause e))))))

(defn- arr [& xs] (ImmutableIntArray/of (int-array xs)))
(defn- elems [^ImmutableIntArray a] (vec (for [i (range (.length a))] (.get a i))))

(deftest abi-one-final-class-and-static-methods
  (let [cls (load-class "function f(x: int) returns r: int { r = x; } function g() returns r: int { r = 1; }")]
    (is (Modifier/isPublic (.getModifiers cls)))
    (is (Modifier/isFinal (.getModifiers cls)))
    (is (= #{"f" "g"} (set (map #(.getName %) (.getDeclaredMethods cls)))))
    (is (every? #(Modifier/isStatic (.getModifiers %)) (.getDeclaredMethods cls)))
    (is (= Integer/TYPE (.getReturnType (.getMethod cls "f" (into-array Class [Integer/TYPE])))))))

(deftest arithmetic
  (let [cls (load-class "function f(a: int, b: int) returns r: int { r = -(a + b * 3) - 10 / 2; }
                         function wrap() returns r: int { r = 2147483647 + 1; }")]
    (is (= -18 (call cls "f" (int 1) (int 4))))
    (is (= Integer/MIN_VALUE (call cls "wrap")))))

(deftest if-else-and-boolean-conditions
  (let [cls (load-class "function mx(a: int, b: int) returns r: int { if (a > b) r = a; else r = b; }
                         function cond(a: int) returns r: int {
                           if ((a > 0 and a < 10) or not a != -5) r = 1; else r = 0;
                         }
                         function impl(a: int, b: int) returns r: int { if (a > 0 -> b > 0) r = 1; }")]
    (is (= 7 (call cls "mx" (int 7) (int 3))))
    (is (= 7 (call cls "mx" (int 3) (int 7))))
    (is (= [1 1 0 0 0] (mapv #(call cls "cond" (int %)) [5 -5 10 0 -4])))
    (is (= [1 0 1 1] (mapv #(call cls "impl" (int %1) (int %2)) [1 1 -1 -1] [1 -1 1 -1])) "ложная посылка -> 1")))

(deftest while-sum
  (let [cls (load-class "function sum(n: int) returns s: int uses i { i = 1; while (i <= n) { s = s + i; i = i + 1; } }")]
    (is (= 5050 (call cls "sum" (int 100))))
    (is (= 0 (call cls "sum" (int 0))))))

(deftest recursion
  (let [cls (load-class "function fact(n: int) returns r: int { if (n <= 1) r = 1; else r = n * fact(n - 1); }
                         function fib(n: int) returns r: int { if (n < 2) r = n; else r = fib(n - 1) + fib(n - 2); }")]
    (is (= 3628800 (call cls "fact" (int 10))))
    (is (= 55 (call cls "fib" (int 10))))))

(deftest arrays-read-length-and-functional-update
  (let [cls (load-class "function sum(a: int[]) returns s: int uses i {
                           i = 0; while (i < length(a)) { s = s + a[i]; i = i + 1; } }
                         function upd(a: int[], i: int, v: int) returns b: int[] { b = a; b[i] = v; }")
        a (arr 1 2 3)
        b (call cls "upd" a (int 1) (int 42))]
    (is (= 6 (call cls "sum" a)))
    (is (= [1 42 3] (elems b)))
    (is (= [1 2 3] (elems a)) "исходный массив не изменился")
    (is (not (identical? a b)))))

(deftest write-to-unassigned-array-is-rejected
  (let [r (c/compile-source "function mk(n: int) returns a: int[] { a[0] = n; }")
        e (first (:errors r))]
    (is (nil? (:bytes r)))
    (is (= :uninitialized-array (:kind e)))
    (is (= :semantic (:phase e)))
    (is (= [1 40] [(:line e) (:column e)]))))

(deftest write-to-array-after-assignment-is-allowed
  (let [cls (load-class "function mk(s: int[], n: int) returns a: int[] { a = s; a[0] = n; a[1] = n + 1; }")]
    (is (= [7 8 3] (elems (call cls "mk" (arr 1 2 3) (int 7)))))))

(deftest array-assignment-in-branches
  (testing "присвоено в обеих ветках — запись допустима"
    (is (nil? (:errors (c/compile-source "function f(s: int[], c: int) returns a: int[] {
                                            if (c > 0) a = s; else a = s; a[0] = 1; }")))))
  (testing "присвоено только в одной ветке (консервативно) — ошибка"
    (is (= :uninitialized-array
           (:kind (first (:errors (c/compile-source "function f(s: int[], c: int) returns a: int[] {
                                                       if (c > 0) a = s; a[0] = 1; }")))))))
  (testing "присвоено только в теле while — ошибка"
    (is (= :uninitialized-array
           (:kind (first (:errors (c/compile-source "function f(s: int[], c: int) returns a: int[] {
                                                       while (c > 0) { a = s; i = 0; } a[0] = 1; }"))))))))

(def ^:private examples-dir "examples/")

(deftest fill-examples
  (testing "fill_ok: массив из src, a[0] = n, результат — обновлённая копия"
    (let [cls (load-class (slurp (str examples-dir "fill_ok.funny")))
          src (arr 1 2 3)
          a (call cls "fill" src (int 10))]
      (is (= [10 2 3] (elems a)))
      (is (= [1 2 3] (elems src)) "параметр не изменился")
      (is (= 15 (call cls "sum" a)))
      (is (= [7 1] (vec (call cls "divmod" (int 15) (int 2)))))))
  (testing "fill_bad: ошибка :uninitialized-array с позицией, байт-кода нет"
    (let [r (c/compile-source (slurp (str examples-dir "fill_bad.funny")))
          e (first (:errors r))]
      (is (nil? (:bytes r)))
      (is (= :uninitialized-array (:kind e)))
      (is (= :semantic (:phase e)))
      (is (= [12 3] [(:line e) (:column e)])))))

(def ^:private differential-source
  "function f(a: int, b: int) returns r: int uses t, u {
     t = 2 + 3 * 4; u = a * 1 + 0; r = t + u - b * 0;
     if (2 < 3) r = r + 1; else r = r / 0; }
   function g(x: int, y: int) returns r: int { r = x / y; }
   function sum(n: int) returns s: int uses i, d { i = 1; d = 7; while (i <= n) { s = s + i; i = i + 1; } }
   function upd(a: int[], i: int, v: int) returns b: int[] { b = a; b[i] = v + 0; }
   function at(a: int[], i: int) returns r: int { r = a[i] + a[i] + length(a); }
   function dm(x: int, y: int) returns q: int, r: int { q = x / y; r = x - q * y; }
   function fact(n: int) returns r: int { if (n <= 1) r = 1; else r = n * fact(n - 1); }
   function dead(a: int) returns r: int uses z { z = a / 0; r = 1; }")

(defn- outcome
  "Результат вызова или класс исключения; массивы приводятся к вектору."
  [cls name & args]
  (try (let [v (apply call cls name args)]
         (cond (instance? ImmutableIntArray v) (elems v)
               (.isArray (class v)) (vec v)
               :else v))
       (catch Throwable e (class e))))

(deftest optimizations-do-not-change-behaviour
  (let [load #(:class (c/load-module differential-source "funny.Module" %))
        variants [(load {:optimize false}) (load {}) (load {:cse true})
                  (load {:const-prop false}) (load {:dce false})]
        calls [["f" (int 3) (int 4)] ["f" (int -5) (int 0)]
               ["g" (int 7) (int 2)] ["g" (int 7) (int 0)]
               ["sum" (int 10)] ["sum" (int 0)]
               ["upd" (arr 1 2 3) (int 1) (int 9)] ["upd" (arr 1 2 3) (int 3) (int 9)]
               ["at" (arr 4 5) (int 1)] ["at" (arr 4 5) (int -1)]
               ["dm" (int 23) (int 7)] ["dm" (int 1) (int 0)]
               ["fact" (int 6)] ["dead" (int 1)]]]
    (doseq [[name & args] calls]
      (let [results (map #(apply outcome % name args) variants)]
        (is (apply = results) (str name " " (pr-str (rest args)) " -> " (pr-str results)))))
    (is (= 18 (outcome (first variants) "f" (int 3) (int 4))) "f(3,4): t = 14, u = 3, r = 17 + 1")
    (is (= ArithmeticException (outcome (second variants) "dead" (int 1)))
        "деление на 0 не удаляется DCE, даже если результат не используется")))

(deftest optimizer-shrinks-ir
  (let [src "function f(a: int) returns r: int uses t { t = 2 + 3 * 4; r = a * 1 + t - 14; }"
        count-instrs #(count (remove (comp #{:label} first) (:code (first (:ir %)))))
        off (c/compile-source src "funny.Module" {:optimize false})
        on (c/compile-source src)]
    (is (< (count-instrs on) (count-instrs off)))))

(deftest tuple-return
  (let [cls (load-class "function divmod(x: int, y: int) returns q: int, r: int { q = x / y; r = x - q * y; }
                         function use(x: int) returns s: int uses q, r { q, r = divmod(x, 7); s = q * 100 + r; }")]
    (is (= [3 2] (vec (call cls "divmod" (int 23) (int 7)))) "int[] в порядке returns")
    (is (= 302 (call cls "use" (int 23))))))

(deftest tuple-with-array
  (let [cls (load-class "function ia(a: int[], n: int) returns r: int, b: int[] { r = n * 2; b = a; b[0] = n; }
                         function ai(a: int[], n: int) returns b: int[], r: int { b = a; b[0] = n; r = length(a); }
                         function pair(a: int[]) returns x: int[], y: int[] { x = a; y = a; y[0] = 9; }
                         function use(a: int[], n: int) returns s: int uses r, b, c, d {
                           r, b = ia(a, n); c, d = ai(b, r); s = r + d + c[0] + b[0] + length(b); }")
        src (arr 1 2 3)
        ia (call cls "ia" src (int 5))
        ai (call cls "ai" src (int 5))]
    (is (= Object/1 (class ia)) "Object[] для кортежа с массивом")
    (is (= [Integer ImmutableIntArray] (mapv class ia)))
    (is (= 10 (aget ^"[Ljava.lang.Object;" ia 0)))
    (is (= [5 2 3] (elems (aget ^"[Ljava.lang.Object;" ia 1))))
    (is (= [ImmutableIntArray Integer] (mapv class ai)))
    (is (= [[5 2 3] 3] [(elems (aget ^"[Ljava.lang.Object;" ai 0)) (aget ^"[Ljava.lang.Object;" ai 1)]))
    (is (= [[1 2 3] [9 2 3]] (mapv elems (call cls "pair" src))))
    (is (= [1 2 3] (elems src)) "исходный массив не изменился")
    (is (= 31 (call cls "use" src (int 5))))))

(deftest int-tuple-stays-int-array
  (let [cls (load-class "function dm(x: int, y: int) returns q: int, r: int { q = x / y; r = x - q * y; }")]
    (is (= (Class/forName "[I") (.getReturnType (.getMethod cls "dm" (into-array Class [Integer/TYPE Integer/TYPE])))))))

(deftest runtime-errors-are-jvm-exceptions
  (let [cls (load-class "function div(a: int, b: int) returns r: int { r = a / b; }
                         function at(a: int[], i: int) returns r: int { r = a[i]; }
                         function put(a: int[], i: int) returns b: int[] { b = a; b[i] = 1; }")]
    (is (thrown? ArithmeticException (call cls "div" (int 1) (int 0))))
    (is (thrown? ArrayIndexOutOfBoundsException (call cls "at" (arr 1 2) (int 2))))
    (is (thrown? ArrayIndexOutOfBoundsException (call cls "at" (arr 1 2) (int -1))))
    (is (thrown? ArrayIndexOutOfBoundsException (call cls "put" (arr 1 2) (int 5))))))

(deftest write-to-parameter-is-a-semantic-error
  (testing "int-параметр"
    (let [r (c/compile-source "function f(a: int) returns r: int { a = 1; r = a; }")]
      (is (nil? (:bytes r)))
      (is (= :read-only (:kind (first (:errors r)))))))
  (testing "параметр-массив: a[i] = v тоже запрещён"
    (let [r (c/compile-source "function f(a: int[]) returns r: int { a[0] = 1; r = 1; }")]
      (is (nil? (:bytes r)))
      (is (= :read-only (:kind (first (:errors r))))))))

(deftest errors-are-returned-as-data
  (is (= :syntax (:phase (first (:errors (c/compile-source "function f( returns r: int { r = 1; }"))))))
  (is (= :type-mismatch (:kind (first (:errors (c/compile-source "function f(a: int[]) returns r: int { r = a; }")))))))

(ns hw4.opt-test
  (:require [clojure.test :refer [deftest is testing]]
            [hw3.types :as types]
            [hw4.cfg :as cfg]
            [hw4.interp :as interp]
            [hw4.ir :as ir]
            [hw4.opt :as opt]))

(defn- fn-of [code & {:keys [params returns] :or {params [] returns ["r"]}}]
  {:name "f" :params params :returns returns :var-types {} :code code})

(defn- run-pass
  "Применяет проход к одному блоку instrs."
  [pass instrs] (pass instrs))


(deftest const-prop-test
  (is (= [[:mov "a" 2] [:mov "b" 5] [:mov "c" 7] [:ret]]
         (opt/const-prop-block [[:mov "a" 2] [:mov "b" 5] [:bin :add "c" "a" "b"] [:ret]])))
  (testing "wraparound"
    (is (= [[:mov "c" -2147483648]]
           (opt/const-prop-block [[:bin :add "c" 2147483647 1]])))
    (is (= [[:mov "c" -2147483648]] (opt/const-prop-block [[:neg "c" -2147483648]])))
    (is (= [[:mov "c" -2147483648]] (opt/const-prop-block [[:bin :div "c" -2147483648 -1]])))
    (is (= [[:mov "c" 0]] (opt/const-prop-block [[:bin :mul "c" 65536 65536]]))))
  (testing "деление усекает к нулю"
    (is (= [[:mov "c" -3]] (opt/const-prop-block [[:bin :div "c" -7 2]]))))
  (testing "деление на 0 не сворачивается"
    (is (= [[:bin :div "c" 5 0]] (opt/const-prop-block [[:bin :div "c" 5 0]])))
    (is (= [[:mov "z" 0] [:bin :div "c" "x" 0]]
           (opt/const-prop-block [[:mov "z" 0] [:bin :div "c" "x" "z"]]))))
  (testing "переопределение убивает факт"
    (is (= [[:mov "a" 1] [:bin :add "a" 1 "x"] [:bin :add "b" "a" 1]]
           (opt/const-prop-block [[:mov "a" 1] [:bin :add "a" "a" "x"] [:bin :add "b" "a" 1]]))))
  (testing "dst не заменяется"
    (is (= [[:mov "a" 1] [:mov "a" 2]]
           (opt/const-prop-block [[:mov "a" 1] [:mov "a" 2]]))))
  (testing "cjmp с константами"
    (is (= [[:mov "a" 1] [:jmp "T"]]
           (opt/const-prop-block [[:mov "a" 1] [:cjmp :lt "a" 2 "T" "F"]])))
    (is (= [[:jmp "F"]] (opt/const-prop-block [[:cjmp :ge 1 2 "T" "F"]]))))
  (testing "копии"
    (is (= [[:mov "x" "y"] [:bin :add "z" "y" 1] [:mov "y" 5] [:bin :add "w" "x" 1]]
           (opt/const-prop-block [[:mov "x" "y"] [:bin :add "z" "x" 1] [:mov "y" 5]
                                  [:bin :add "w" "x" 1]]))
        "после переопределения y копия x=y перестаёт действовать")))

(deftest algebra-test
  (doseq [[in out] [[[:bin :add "d" "x" 0] [:mov "d" "x"]]
                    [[:bin :add "d" 0 "x"] [:mov "d" "x"]]
                    [[:bin :sub "d" "x" 0] [:mov "d" "x"]]
                    [[:bin :mul "d" "x" 1] [:mov "d" "x"]]
                    [[:bin :mul "d" 1 "x"] [:mov "d" "x"]]
                    [[:bin :div "d" "x" 1] [:mov "d" "x"]]
                    [[:bin :mul "d" "x" 0] [:mov "d" 0]]
                    [[:bin :mul "d" 0 "x"] [:mov "d" 0]]
                    [[:bin :sub "d" "x" "x"] [:mov "d" 0]]
                    [[:bin :div "d" 0 "x"] [:bin :div "d" 0 "x"]]
                    [[:bin :div "d" "x" "x"] [:bin :div "d" "x" "x"]]
                    [[:bin :sub "d" 0 "x"] [:bin :sub "d" 0 "x"]]
                    [[:cjmp :eq "x" "x" "T" "F"] [:jmp "T"]]
                    [[:cjmp :le "x" "x" "T" "F"] [:jmp "T"]]
                    [[:cjmp :ge "x" "x" "T" "F"] [:jmp "T"]]
                    [[:cjmp :ne "x" "x" "T" "F"] [:jmp "F"]]
                    [[:cjmp :lt "x" "x" "T" "F"] [:jmp "F"]]
                    [[:cjmp :gt "x" "x" "T" "F"] [:jmp "F"]]]]
    (is (= [out] (opt/algebra-block [in])) (pr-str in))))

(deftest dce-test
  (let [g (cfg/build [[:bin :add "t" "x" 1]
                      [:bin :add "u" "t" 1]
                      [:mov "r" "x"]
                      [:ret]])
        g' (opt/dce g ["r"])]
    (is (= [[:mov "r" "x"] [:ret]] (get-in g' [:blocks (:entry g) :instrs]))
        "цепочка мёртвых вычислений удаляется до неподвижной точки"))
  (testing "опасное и вызовы остаются"
    (let [code [[:bin :div "t" "x" "y"] [:aget "u" "a" 0] [:aset "v" "a" 0 1]
                [:call ["w"] "g" ["x"]] [:len "q" "a"] [:mov "r" 1] [:ret]]
          g' (opt/dce (cfg/build code) ["r"])]
      (is (= [[:bin :div "t" "x" "y"] [:aget "u" "a" 0] [:aset "v" "a" 0 1]
              [:call ["w"] "g" ["x"]] [:mov "r" 1] [:ret]]
             (get-in g' [:blocks (:entry g') :instrs])))))
  (testing "живость между блоками"
    (let [g (cfg/build [[:mov "a" 1] [:mov "b" 2]
                        [:cjmp :lt "x" 0 "L1" "L2"]
                        [:label "L1"] [:mov "r" "a"] [:ret]
                        [:label "L2"] [:mov "r" 0] [:ret]])
          g' (opt/dce g ["r"])]
      (is (= [[:mov "a" 1] [:cjmp :lt "x" 0 "L1" "L2"]]
             (get-in g' [:blocks (:entry g) :instrs]))))))

(deftest cse-test
  (is (= [[:bin :add "t1" "a" "b"] [:mov "t2" "t1"]]
         (opt/cse-block [[:bin :add "t1" "a" "b"] [:bin :add "t2" "a" "b"]])))
  (testing "коммутативность"
    (is (= [[:bin :mul "t1" "a" "b"] [:mov "t2" "t1"]]
           (opt/cse-block [[:bin :mul "t1" "a" "b"] [:bin :mul "t2" "b" "a"]])))
    (is (= [[:bin :sub "t1" "a" "b"] [:bin :sub "t2" "b" "a"]]
           (opt/cse-block [[:bin :sub "t1" "a" "b"] [:bin :sub "t2" "b" "a"]]))))
  (testing "операнд переопределён"
    (is (= [[:bin :add "t1" "a" "b"] [:mov "a" 0] [:bin :add "t2" "a" "b"]]
           (opt/cse-block [[:bin :add "t1" "a" "b"] [:mov "a" 0] [:bin :add "t2" "a" "b"]]))))
  (testing "результат перезаписан"
    (is (= [[:bin :add "t1" "a" "b"] [:mov "t1" 0] [:bin :add "t2" "a" "b"]]
           (opt/cse-block [[:bin :add "t1" "a" "b"] [:mov "t1" 0] [:bin :add "t2" "a" "b"]]))))
  (testing "dst совпадает с операндом"
    (is (= [[:bin :add "a" "a" "b"] [:bin :add "t" "a" "b"]]
           (opt/cse-block [[:bin :add "a" "a" "b"] [:bin :add "t" "a" "b"]]))))
  (testing "neg, len, aget"
    (is (= [[:neg "t1" "a"] [:mov "t2" "t1"]] (opt/cse-block [[:neg "t1" "a"] [:neg "t2" "a"]])))
    (is (= [[:len "t1" "a"] [:mov "t2" "t1"]] (opt/cse-block [[:len "t1" "a"] [:len "t2" "a"]])))
    (is (= [[:aget "t1" "a" "i"] [:mov "t2" "t1"]]
           (opt/cse-block [[:aget "t1" "a" "i"] [:aget "t2" "a" "i"]])))
    (is (= [[:aget "t1" "a" "i"] [:aset "a" "a" "i" 3] [:aget "t2" "a" "i"]]
           (opt/cse-block [[:aget "t1" "a" "i"] [:aset "a" "a" "i" 3] [:aget "t2" "a" "i"]]))
        "aset переопределяет a")))


(deftest course-examples-test
  (let [f (fn-of [[:bin :add "t1" "a" "b"] [:bin :add "t2" "a" "b"] [:bin :add "r" "t1" "t2"] [:ret]]
                 :params ["a" "b"])
        count-ab (fn [g] (count (filter #(and (= :bin (first %)) (= :add (second %))
                                              (= ["a" "b"] (subvec % 3))) (:code g))))
        without (opt/optimize-function f {:cse false})
        with (opt/optimize-function f {:cse true})]
    (is (= 2 (count-ab without)))
    (is (= 1 (count-ab with)))
    (is (= [[:bin :add "t1" "a" "b"] [:bin :add "r" "t1" "t1"] [:ret]]
           (remove #(= :label (first %)) (:code with)))
        "CSE + копии + DCE: t2 исчезает")
    (is (< (:instructions (opt/metrics with)) (:instructions (opt/metrics without)))))
  (let [f (fn-of [[:mov "t1" 1] [:bin :add "t2" "t1" 0] [:mov "r" "t2"] [:ret]])
        g (opt/optimize-function f {})]
    (is (= [[:mov "r" 1] [:ret]] (remove #(= :label (first %)) (:code g))))))

(deftest unreachable-removed-test
  (let [f (fn-of [[:mov "x" 1] [:cjmp :lt "x" 2 "A" "B"]
                  [:label "A"] [:mov "r" 1] [:ret]
                  [:label "B"] [:mov "r" 2] [:ret]])
        g (opt/optimize-function f {})]
    (is (= [[:mov "r" 1] [:ret]] (remove #(= :label (first %)) (:code g))))
    (is (= 1 (count (filter #(= :label (first %)) (:code g)))) "только A: у входа метки нет")
    (is (= [[:label "B"] [:mov "r" 2] [:ret]]
           (let [c (:code (opt/optimize-function f {:const-prop false :algebra false :dce false}))]
             (vec (drop 5 c)))))))


(def programs
  [["loop"
    "function f(a: int[], n: int) returns r: int uses i { r = 0; i = 0; while (i < n) { r = r + a[i]; i = i + 1; } }"
    [[[1 2 3] 3] [[1 2 3] 0] [[1 2 3] 4] [[] 0] [[5] -1] [[2147483647 1] 2]]]
   ["ifelse"
    "function f(x: int, y: int) returns r: int uses t { t = x * 2 + 0; if (t > y) { r = t - y; } else { r = y - t; } r = r * 1 + (x - x); }"
    [[3 4] [10 1] [0 0] [2147483647 -5] [-2147483648 7]]]
   ["andor"
    "function f(x: int, y: int) returns r: int { r = 0; if ((x > 0 and y > 0) or x == y) { r = 1; } if (not (x < y) and (y != 3 or x == 3)) { r = r + 10; } }"
    [[1 1] [0 0] [-1 -1] [3 3] [5 3] [2 3] [3 5]]]
   ["array-rw"
    "function f(a: int[], i: int) returns r: int uses b { b = a; b[i] = a[i] + a[i]; r = b[i] + a[i] + length(a); }"
    [[[1 2 3] 0] [[1 2 3] 2] [[1 2 3] 3] [[1 2 3] -1] [[] 0]]]
   ["tuple-call"
    "function g(x: int, y: int) returns q: int, w: int { q = x + y; w = x * y; }\nfunction f(x: int, y: int) returns r: int uses a, b { a, b = g(x, y); r = a + b + (a + b); }"
    [[1 2] [0 0] [2147483647 2] [-3 4]]]
   ["recursion"
    "function f(n: int) returns r: int uses t { if (n <= 1) { r = 1; } else { t = f(n - 1); r = n * t; } }"
    [[0] [1] [5] [13] [20]]]
   ["div"
    "function f(x: int, y: int) returns r: int uses t { t = x / y; r = x / y + 0; r = r + 1 * t; if (y == 0) { r = r + 1; } }"
    [[10 2] [10 0] [0 0] [-7 2] [-2147483648 -1] [7 -2]]]
   ["dead-div"
    "function f(x: int, y: int) returns r: int uses t { t = x / y; r = 5; }"
    [[1 0] [1 1]]]
   ["mul-zero-div"
    "function f(x: int, y: int) returns r: int { r = (x / y) * 0; }"
    [[1 0] [1 1]]]
   ["oob"
    "function f(a: int[], i: int) returns r: int uses t { t = a[i]; r = 3; }"
    [[[1 2] 0] [[1 2] 2] [[1 2] -1]]]
   ["const-branch"
    "function f(x: int) returns r: int uses k { k = 3; if (k > 2) { r = x + k * 2; } else { r = x / 0; } }"
    [[1] [2147483647]]]
   ["cse-candidate"
    "function f(a: int[], i: int, x: int) returns r: int uses s, t { s = a[i] + x * 3; t = a[i] + 3 * x; r = s * t - (s - t); a[i] = 7; r = r + a[i] + a[i]; }"
    [[[1 2 3] 1 4] [[1 2 3] 5 4] [[100000 2] 0 100000]]]])

(def flag-combos
  (for [cp [true false] al [true false] d [true false] c [true false]]
    {:const-prop cp :algebra al :dce d :cse c}))

(defn- module-of [src] (ir/gen-module (:ast (types/check-source src))))

(deftest semantic-preservation-test
  (let [comparisons (atom 0)]
    (doseq [[pname src inputs] programs
            :let [module (module-of src)]]
      (is (seq module) pname)
      (doseq [opts flag-combos
              :let [opt-module (opt/optimize-module module opts)]
              args inputs]
        (swap! comparisons inc)
        (is (= (interp/outcome module "f" args) (interp/outcome opt-module "f" args))
            (str pname " " opts " " args))))
    (println "comparisons:" @comparisons)))

(deftest errors-preserved-test
  (let [m (module-of (second (first (filter #(= "div" (first %)) programs))))
        div-src (module-of "function f(x: int, y: int) returns r: int uses t { t = x / y; r = 5; }")
        oob (module-of "function f(a: int[], i: int) returns r: int uses t { t = a[i]; r = 3; }")]
    (is (= {:error :div-zero}
           (interp/outcome (opt/optimize-module div-src {:cse true}) "f" [1 0])))
    (is (= {:error :index}
           (interp/outcome (opt/optimize-module oob {:cse true}) "f" [[1 2] 5])))
    (is (= {:error :div-zero} (interp/outcome (opt/optimize-module m {:cse true}) "f" [10 0])))))


(defn- rand-expr [^java.util.Random rnd depth]
  (if (or (zero? depth) (< (.nextInt rnd 4) 1))
    (case (.nextInt rnd 4)
      0 "x" 1 "y"
      2 (str (- (.nextInt rnd 9) 3))
      3 (str ([2147483647 -2147483648 65536 -1] (.nextInt rnd 4))))
    (let [op (["+" "-" "*" "/"] (.nextInt rnd 4))]
      (str "(" (rand-expr rnd (dec depth)) " " op " " (rand-expr rnd (dec depth)) ")"))))

(deftest random-expressions-test
  (let [rnd (java.util.Random. 42)
        comparisons (atom 0)
        inputs [[0 0] [1 2] [-3 5] [2147483647 1] [-2147483648 -1] [4 0] [0 -3]]]
    (dotimes [_ 200]
      (let [e (rand-expr rnd 4)
            src (str "function f(x: int, y: int) returns r: int { r = " e "; }")
            module (module-of src)]
        (when-not (seq module) (is false (str "не компилируется: " src)))
        (doseq [opts [{} {:cse true} {:const-prop false} {:algebra false :cse true}
                      {:const-prop false :algebra false :dce false :cse true}]
                :let [om (opt/optimize-module module opts)]
                args inputs]
          (swap! comparisons inc)
          (when-not (= (interp/outcome module "f" args) (interp/outcome om "f" args))
            (is false (str src " " opts " " args))))))
    (println "comparisons:" @comparisons)
    (is true)))


(deftest metrics-test
  (let [src "function f(x: int, y: int) returns r: int uses k { k = 2 + 3; r = (x + y) * (x + y) + k * 0 + (x * 1); }"
        f (first (module-of src))
        base (opt/metrics f)]
    (is (= {:instructions 1 :temps 0} (opt/metrics (fn-of [[:label "B0"] [:mov "r" 1]]))))
    (is (= {:instructions 2 :temps 2}
           (opt/metrics (fn-of [[:bin :add "%t1" "x" 1] [:bin :add "%t2" "%t1" "%t1"]]))))
    (doseq [opts flag-combos]
      (is (<= (:instructions (opt/metrics (opt/optimize-function f opts))) (:instructions base)) (str opts)))
    (is (< (:instructions (opt/metrics (opt/optimize-function f {:cse true})))
           (:instructions (opt/metrics (opt/optimize-function f {}))))
        "CSE даёт выигрыш")
    (is (< (:instructions (opt/metrics (opt/optimize-function f {}))) (:instructions base)))
    (is (< (:temps (opt/metrics (opt/optimize-function f {}))) (:temps base)))))

(deftest optimization-preserves-entry-before-explicit-b0
  (let [f (fn-of [[:mov "r" 3] [:label "B0"] [:bin :sub "r" "r" 1]
                  [:cjmp :ne "r" 0 "B0" "B1"] [:label "B1"] [:ret]])
        graph (cfg/build (:code f))
        optimized (opt/optimize-function f {})]
    (is (apply distinct? (:order graph)))
    (is (= [0] (interp/run [optimized] "f" [] 100)))))

(deftest optimized-types-follow-surviving-temporaries
  (let [f (assoc (fn-of [[:bin :mul "%t1" "x" 3] [:bin :add "%t2" "x" 2]
                         [:bin :mul "r" "%t2" "x"] [:ret]] :params ["x"])
                 :var-types {"x" :int "r" :int "unused" :int-array "%t1" :int "%t2" :int})
        optimized (opt/optimize-function f {})]
    (is (= (dissoc (:var-types f) "%t1") (:var-types optimized)))
    (is (= [15] (interp/run [optimized] "f" [3])))))

(ns hw4.regalloc-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hw3.types :as types]
            [hw4.ir :as ir]
            [hw4.regalloc :as ra]))

(defn- fn-of [code & {:keys [params returns] :or {params [] returns []}}]
  {:name "f" :params params :returns returns :var-types {} :code code})

(defn- edge? [g a b] (contains? (get-in g [:edges a]) b))

(defn- well-formed? [{:keys [nodes edges] :as g}]
  (and (= (set nodes) (set (keys edges)))
       (every? (fn [[a ns]] (and (not (contains? ns a))
                                 (every? #(edge? g % a) ns)))
               edges)))

(defn- funny-fns [src]
  (let [{:keys [ast errors]} (types/check-source src)
        fns (ir/gen-module ast)]
    (assert (empty? errors) (pr-str errors))
    (assert (seq fns) "программа не скомпилировалась в IR")
    fns))

(deftest straight-line
  (let [f (fn-of [[:bin :add "%t1" "a" "b"] [:bin :add "%t2" "a" "b"]
                  [:bin :add "r" "%t1" "%t2"] [:ret]]
                 :params ["a" "b"] :returns ["r"])
        res (ra/allocate f)]
    (is (= ["%t1" "%t2"] (ra/temps f)))
    (is (edge? (:graph res) "%t1" "%t2"))
    (is (= 2 (:num-colors res)))
    (is (ra/valid-coloring? (:graph res) (:colors res)))))

(deftest sequential-share-color
  (let [f (fn-of [[:bin :add "%t1" "a" 1] [:mov "x" "%t1"]
                  [:bin :add "%t2" "x" 2] [:mov "y" "%t2"]
                  [:bin :add "r" "x" "y"] [:ret]]
                 :params ["a"] :returns ["r"])
        res (ra/allocate f)]
    (is (empty? (get-in res [:graph :edges "%t1"])))
    (is (= 1 (:num-colors res)))
    (is (= 2 (:temps res)))))

(deftest mov-coalescing
  (let [copy [[:bin :add "%t1" "a" 1] [:mov "%t2" "%t1"] [:bin :add "r" "%t2" "%t1"] [:ret]]
        redefined [[:bin :add "%t1" "a" 1] [:mov "%t2" "%t1"] [:bin :add "%t1" "%t1" 5]
                   [:bin :add "r" "%t2" "%t1"] [:ret]]
        fx #(fn-of % :params ["a"] :returns ["r"])]
    (is (not (edge? (ra/interference-graph (fx copy)) "%t1" "%t2")))
    (is (= 1 (:num-colors (ra/allocate (fx copy)))))
    (is (edge? (ra/interference-graph (fx redefined)) "%t1" "%t2")
        "t1 переопределяется, пока копия t2 жива")))

(deftest loop-back-edge
  (let [f (fn-of [[:bin :add "%t1" "n" 1]
                  [:mov "i" 0]
                  [:label "L1"]
                  [:cjmp :lt "i" "n" "L2" "L3"]
                  [:label "L2"]
                  [:bin :add "%t2" "i" 1]
                  [:mov "i" "%t2"]
                  [:jmp "L1"]
                  [:label "L3"]
                  [:bin :add "r" "i" "%t1"]
                  [:ret]]
                 :params ["n"] :returns ["r"])
        g (ra/interference-graph f)]
    (is (edge? g "%t1" "%t2"))
    (is (= 2 (:num-colors (ra/allocate f))))))

(deftest call-two-temps
  (let [f (fn-of [[:call ["%t1" "%t2"] "g" ["a"]] [:mov "r" 0] [:ret]]
                 :params ["a"] :returns ["r"])
        g (ra/interference-graph f)]
    (is (edge? g "%t1" "%t2"))
    (is (= 2 (:num-colors (ra/allocate f))))))

(deftest no-temps
  (let [f (fn-of [[:mov "r" 1] [:ret]] :returns ["r"])
        res (ra/allocate f)]
    (is (= 0 (:num-colors res)))
    (is (= {:nodes [] :edges {}} (:graph res)))
    (is (= {} (:colors res)))))

(deftest spill-with-k
  (let [f (fn-of [[:bin :add "%t1" "a" 1] [:bin :add "%t2" "a" 2] [:bin :add "%t3" "a" 3]
                  [:bin :add "%t4" "%t1" "%t2"] [:bin :add "%t5" "%t4" "%t3"]
                  [:mov "r" "%t5"] [:ret]]
                 :params ["a"] :returns ["r"])
        full (ra/allocate f)
        k2 (ra/allocate f {:k 2})
        kbig (ra/allocate f {:k 10})]
    (is (= 3 (:num-colors full)))
    (is (not (contains? full :spilled)))
    (is (= 1 (count (:spilled k2))))
    (is (every? #(>= (get-in k2 [:colors %]) 2) (:spilled k2)))
    (is (= [] (:spilled kbig)))))

(deftest text-and-dot
  (let [f (fn-of [[:bin :add "%t1" "a" "b"] [:bin :add "%t2" "a" "b"]
                  [:bin :add "r" "%t1" "%t2"] [:ret]]
                 :params ["a" "b"] :returns ["r"])
        {:keys [graph colors]} (ra/allocate f)
        txt (ra/graph->text graph colors)
        dot (ra/graph->dot "f" graph colors)]
    (is (str/includes? txt "%t1 [color "))
    (is (str/includes? txt "]: %t2"))
    (is (str/starts-with? dot "graph "))
    (is (str/includes? dot "\"%t1\" -- \"%t2\""))))

(deftest valid-coloring-detects-conflict
  (let [g {:nodes ["%t1" "%t2"] :edges {"%t1" #{"%t2"} "%t2" #{"%t1"}}}]
    (is (ra/valid-coloring? g {"%t1" 0 "%t2" 1}))
    (is (not (ra/valid-coloring? g {"%t1" 0 "%t2" 0})))))

(def sources
  ["function f(a: int[], n: int) returns r: int uses i { r = 0; i = 0; while (i < n) { r = r + a[i] * 2 + a[i] * 3; i = i + 1; } }"
   "function g(x: int, y: int) returns r: int { if ((x < y and y > 0) or x == 5) { r = x * y + (x - y) * (x + y); } else { r = (x + 1) * (y + 2) - x / (y + 1); } }"
   "function h(a: int[], n: int) returns s: int uses i, j { s = 0; i = 0; while (i < n) { j = 0; while (j < i) { if (a[i] > a[j] and not (j == 3)) { s = s + a[i] * a[j]; } else { s = s - (a[j] + i) * (j + 1); } j = j + 1; } i = i + 1; } }"])

(deftest compiled-programs
  (is (= 3 (count (mapcat funny-fns sources))) "все три программы реально скомпилировались")
  (is (seq (ra/temps (first (funny-fns (first sources))))))
  (doseq [src sources
          f (funny-fns src)]
    (let [r1 (ra/allocate f)
          r2 (ra/allocate f)
          g (:graph r1)]
      (testing (:name f)
        (is (well-formed? g))
        (is (ra/valid-coloring? g (:colors r1)))
        (is (<= (:num-colors r1) (count (ra/temps f))))
        (is (= (:colors r1) (:colors r2)))
        (is (= (:graph r1) (:graph r2)))
        (is (every? #(str/starts-with? % "%") (:nodes g)))))))

(deftest large-chain
  (let [n 40
        t #(str "%t" %)
        code (vec (concat
                   (for [i (range 1 4)] [:bin :add (t i) "a" i])
                   (for [i (range 4 (inc n))] [:bin :add (t i) (t (dec i)) (t (- i 3))])
                   [[:mov "r" (t n)] [:ret]]))
        f (fn-of code :params ["a"] :returns ["r"])
        res (ra/allocate f)]
    (is (= n (:temps res)))
    (is (well-formed? (:graph res)))
    (is (ra/valid-coloring? (:graph res) (:colors res)))
    (is (< (:num-colors res) n))
    (is (<= 3 (:num-colors res) 5))))

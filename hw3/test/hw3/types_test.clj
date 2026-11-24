(ns hw3.types-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hw3.types :as types]))

(defn- errors [source] (:errors (types/check-source source)))

(defn- wrap [uses body]
  (str "function f(p: int, a: int[]) returns r: int" uses " {\n" body "\n}\n"))

(defn- expect-error
  "Проверяет ровно одну ошибку вида `kind` в позиции line/column, сообщение которой содержит `fragment`."
  [source kind line column fragment]
  (let [errs (errors source)]
    (is (= 1 (count errs)) (pr-str errs))
    (let [e (first errs)]
      (is (= kind (:kind e)))
      (is (= [line column] [(:line e) (:column e)]))
      (is (str/includes? (:message e) fragment) (:message e)))))

(defn- find-nodes [pred tree]
  (cond-> (vec (mapcat #(find-nodes pred %)
                       (cond (map? tree) (vals tree) (sequential? tree) tree :else [])))
    (and (map? tree) (pred tree)) (conj tree)))

;; положительные

(deftest valid-program-has-no-errors-and-typed-expressions
  (let [{:keys [ast errors]} (types/check-source (slurp "examples/ok.funny"))
        exprs (find-nodes #(and (:node %) (contains? #{:int :bool :name :unary :binary :index :call} (:node %))) ast)]
    (is (empty? errors))
    (is (seq exprs))
    (is (every? #(contains? #{:int :int-array :bool :tuple} (:type %)) exprs))))

(deftest annotated-ast-format
  (let [{:keys [ast]} (types/check-source
                       (wrap "" "if (p < 1 and not (p == 2)) r = length(a); else r = a[p] + 1;"))
        f (first (:functions ast))
        stmt (first (:statements (:body f)))
        cond-node (:condition stmt)]
    (is (= {:params [:int :int-array] :returns [:int]} (:signature f)))
    (is (= :bool (:type cond-node)))
    (is (= :int (:type (:left (:left cond-node)))))
    (is (= :param (:symbol (:left (:left cond-node)))))
    (is (= :int-array (:type (:target (:left (:value (:else stmt)))))))))

(deftest tuple-assignment-and-recursion
  (is (empty? (errors (str "function g(x: int) returns q: int, w: int { q = x; w = x; }\n"
                           "function h(x: int) returns r: int uses a, b { a, b = g(x); r = h(a) + b; }")))))

(deftest contract-scopes
  (is (empty? (errors (str "function f(x: int) requires x > 0 returns r: int ensures r >= x "
                           "{ r = x; while (r < 10) invariant r >= x r = r + 1; }")))))

(deftest locals-with-explicit-type
  (is (empty? (errors (wrap " uses t: int, b: int[]" "t = p; b = a; r = t + b[0];")))))

;; бонус: вывод типов

(deftest infers-local-types-from-first-assignment
  (let [{:keys [ast errors]} (types/check-source
                              (wrap " uses t, b, c" "t = p + 1; b = a; b[0] = t; c[1] = 2; r = t + b[0];"))
        locals (:locals (first (:functions ast)))]
    (is (empty? errors))
    (is (= {"t" :int "b" :int-array "c" :int-array} (into {} (map (juxt :name :type)) locals)))
    (is (every? :inferred? locals))))

;; отрицательные

(deftest negative-single-error-cases
  (doseq [[source kind line column fragment]
          [[(wrap "" "r = zz;") :unknown-name 2 5 "Unknown name 'zz'"]
           [(wrap "" "r = nope(1);") :unknown-function 2 5 "Unknown function 'nope'"]
           [(wrap "" "p = 1; r = 1;") :read-only 2 1 "read-only"]
           [(wrap "" "a[0] = 1; r = 1;") :read-only 2 1 "read-only"]
           [(wrap "" "r = a;") :type-mismatch 2 5 "Cannot assign int[] to 'r'"]
           [(wrap "" "r = p < 1;") :type-mismatch 2 7 "Cannot assign bool"]
           [(wrap "" "r = p + a;") :type-mismatch 2 7 "expects int operands"]
           [(wrap "" "r = a[a];") :type-mismatch 2 7 "index must be int"]
           [(wrap "" "r = p[0];") :type-mismatch 2 6 "Only int[] can be indexed"]
           [(wrap " uses b: int[]" "b[0] = a; r = 1;") :type-mismatch 2 8 "element must be int"]
           [(wrap " uses b: int[]" "b[0][1] = 1; r = 1;") :type-mismatch 2 6 "cannot be indexed again"]
           [(wrap "" "if (p) r = 1;") :type-mismatch 2 5 "Condition must be bool"]
           [(wrap "" "while (p + 1) r = 1;") :type-mismatch 2 10 "Condition must be bool"]
           [(wrap "" "assert p; r = 1;") :type-mismatch 2 8 "must be bool"]
           [(wrap " uses t" "t = p;\nt = a;") :type-mismatch 3 5 "Cannot assign int[] to 't' of type int"]
           [(wrap " uses t" "r = t;\n t = 1;") :cannot-infer 2 5 "read before its first assignment"]
           [(wrap " uses t" "t = p < 1; r = 1;") :cannot-infer 2 7 "bool"]
           [(str "function f(x: int) requires r > 0 returns r: int { r = x; }")
            :not-visible 1 29 "not visible in requires"]
           [(str "function f(x: int) returns r: int ensures r > t uses t: int { t = x; r = t; }")
            :not-visible 1 47 "not visible in ensures"]
           ["function f(x: int, x: int) returns r: int { r = x; }"
            :duplicate-declaration 1 20 "Duplicate declaration of 'x'"]
           ["function f(x: int) returns x: int { }"
            :duplicate-declaration 1 28 "first declared at line 1, column 12"]
           ["function f() returns r: int { r = 1; }\nfunction f() returns r: int { r = 2; }"
            :duplicate-declaration 2 1 "Duplicate function 'f'"]]
          :when line]
    (expect-error source kind line column fragment))
  (doseq [[src n] [["function f() returns r: int { r = 2147483647; }" 0]
                   ["function f() returns r: int { r = -2147483648; }" 0]
                   ["function f() returns r: int { r = 2147483648; }" 1]
                   ["function f() returns r: int { r = 99999999999; }" 1]]]
    (is (= n (count (errors src))) src))
  (is (= 1 (count (errors "function f() returns r: int uses y, y { r = 1; y = 2; }")))
      "дубль локальной даёт одну ошибку, без лишних")
  (let [errs (errors (wrap " uses t" "r = 1;"))]
    (is (= :cannot-infer (:kind (first errs))))
    (is (str/includes? (:message (first errs)) "never assigned")))
  (let [errs (errors (wrap "" "r = 1 and 2;"))]
    (is (str/includes? (:message (first errs)) "'and' expects bool operands, got int and int"))))

(deftest calls-check-arity-and-argument-types
  (let [lib "function g(x: int, y: int[]) returns q: int { q = x; }\n"]
    (expect-error (str lib "function h() returns r: int { r = g(1); }")
                  :arity 2 35 "expects 2 argument(s), got 1")
    (expect-error (str lib "function h() returns r: int uses b: int[] { b[0] = 1; r = g(b, b); }")
                  :type-mismatch 2 61 "Argument 1 of 'g' must be int")
    (expect-error "function h(a: int[]) returns r: int { r = length(1); }"
                  :type-mismatch 1 50 "Argument 1 of 'length' must be int[]")
    (expect-error "function h(a: int[]) returns r: int { r = length(a, a); }"
                  :arity 1 43 "expects 1 argument(s), got 2")))

(deftest multi-value-calls
  (let [lib "function g() returns q: int, w: int { q = 1; w = 2; }\n"]
    (expect-error (str lib "function h() returns r: int { r = g(); }")
                  :type-mismatch 2 35 "multi-value call is allowed only in a tuple assignment")
    (expect-error (str lib "function h() returns r: int, w: int, z: int { r, w, z = g(); }")
                  :arity 2 55 "returns 2 value(s), but 3 target(s) given")))

(deftest scope-of-contracts
  (expect-error "function f(x: int) requires r > 0 returns r: int { r = x; }"
                :not-visible 1 29 "not visible in requires")
  (expect-error "function f(x: int) returns r: int ensures r > t uses t: int { t = x; r = t; }"
                :not-visible 1 47 "not visible in ensures"))

(deftest several-errors-are-reported
  (let [errs (errors (wrap "" "r = zz;\np = 1;\nr = a;"))]
    (is (= 3 (count errs)))
    (is (= [2 3 4] (map :line errs)))
    (is (every? #(and (:line %) (:column %) (seq (:message %))) errs))))

(deftest syntax-errors-are-merged
  (let [errs (errors "function f() returns r: int { r = ; }\nfunction g() returns r: int { r = zz; }")]
    (is (= [:syntax :semantic] (map :phase errs)))))

(deftest no-cascading-errors
  (testing "ошибка в первом присваивании не плодит ошибки про вывод типа"
    (is (= [:unknown-name] (mapv :kind (errors (wrap " uses t" "t = zz; r = t;"))))))
  (testing "после синтаксической ошибки нет ложных :cannot-infer"
    (is (= [:syntax] (mapv :phase (errors (wrap " uses t" "t = 1 r = p;")))))))

(deftest tuple-targets-have-own-positions
  (let [lib "function g() returns a: int, b: int { a = 1; b = 2; }\n"
        errs (errors (str lib "function f(zz: int, yy: int) returns r: int { zz, yy = g(); }"))]
    (is (= [:read-only :read-only] (mapv :kind errs)))
    (is (apply not= (map :column errs)))))

(deftest implicit-locals-without-uses
  (testing "первое присваивание неизвестному имени объявляет локальную (эталоны project/examples)"
    (let [{:keys [ast errors]} (types/check-source
                                "f(n: int) returns r: int { i = 0; r = 0; while (i < n) { r = r + i; i = i + 1; } }")
          locals (:locals (first (:functions ast)))]
      (is (empty? errors))
      (is (= [{:name "i" :type :int :inferred? true}]
             (mapv #(select-keys % [:name :type :inferred?]) locals)))))
  (testing "массив и кортеж"
    (is (empty? (errors "g() returns a: int, b: int { a = 1; b = 2; }\nf() returns r: int { x, y = g(); r = x - y; }")))
    (let [{:keys [ast errors]} (types/check-source "f(a: int[]) returns r: int { b = a; b[0] = 1; r = b[0]; }")]
      (is (empty? errors))
      (is (= [:int-array] (mapv :type (:locals (first (:functions ast))))))))
  (testing "чтение до первого присваивания остаётся ошибкой"
    (is (= [:unknown-name] (mapv :kind (errors "f() returns r: int { r = i; i = 0; }"))))))

(deftest errors-are-sorted-by-position-across-phases
  (let [errs (errors "function f() returns r: int { r = zz; }\nfunction g() returns r: int { r = ; }")]
    (is (= [:semantic :syntax] (mapv :phase errs)))))

(deftest int-literal-range-applies-only-to-code
  (is (empty? (errors "function f(x: int) requires x < 99999999999 returns r: int { r = x; }"))))

(deftest inference-uses-first-correct-assignment
  (testing "ошибочное первое присваивание не мешает выводу по следующему корректному"
    (let [{:keys [ast errors]} (types/check-source (wrap " uses t" "t = missing; t = 7; r = t;"))]
      (is (= [:unknown-name] (mapv :kind errors)))
      (is (= :int (:type (first (:locals (first (:functions ast)))))))))
  (testing "после ошибочного присваивания обновление массива задаёт int[] без вторичных ошибок"
    (let [{:keys [ast errors]} (types/check-source (wrap " uses t" "t = missing; t[0] = 1; r = 1;"))]
      (is (= [:unknown-name] (mapv :kind errors)))
      (is (= :int-array (:type (first (:locals (first (:functions ast)))))))))
  (testing "противоречие с типом, выведенным позже, всё ещё ошибка"
    (is (= [:unknown-name :type-mismatch]
           (mapv :kind (errors (wrap " uses t" "t = missing; t = 7; t = a; r = 1;")))))))

(deftest upstream-syntax-without-function-keyword-is-checked
  (is (empty? (errors "g(x: int) returns r: int { r = x; }\nf() returns r: int uses t { t = g(1); r = t; }")))
  (expect-error "f(x: int) returns r: int { r = zz; }" :unknown-name 1 32 "Unknown name 'zz'"))

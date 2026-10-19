(ns hw2.parser-test
  (:require [clojure.test :refer [deftest is]]
            [hw2.parser :as parser]))

(defn- without-positions [value]
  (cond
    (map? value) (into {} (map (fn [[key child]] [key (without-positions child)]))
                       (dissoc value :position))
    (vector? value) (mapv without-positions value)
    :else value))

(deftest parser-reports-one-based-token-positions
  (let [result (parser/parse-expression "a +\n")
        error (first (:errors result))]
    (is (= {:line 2 :column 1} (select-keys error [:line :column])))))

(deftest parser-builds-arithmetic-ast-with-precedence
  (let [{:keys [ast errors]} (parser/parse-expression "1 + 2 * 3")]
    (is (empty? errors))
    (is (= {:node :binary :operator :PLUS
            :left {:node :int :value 1}
            :right {:node :binary :operator :STAR
                    :left {:node :int :value 2}
                    :right {:node :int :value 3}}}
           (without-positions ast)))))

(deftest parser-parses-array-reads-and-calls
  (let [{:keys [ast errors]} (parser/parse-expression "f(a[0], length(xs))")]
    (is (empty? errors))
    (is (= {:node :call :name "f"
            :args [{:node :index
                    :target {:node :name :name "a"}
                    :index {:node :int :value 0}}
                   {:node :call :name "length"
                    :args [{:node :name :name "xs"}]}]}
           (without-positions ast)))))

(deftest parser-respects-boolean-precedence-and-implication-associativity
  (let [ast (:ast (parser/parse-expression "not x == 0 and y > 0 or true -> a -> b"))]
    (is (= :IMPLIES (:operator ast)))
    (is (= :IMPLIES (get-in ast [:right :operator])))
    (is (= :OR (get-in ast [:left :operator])))
    (is (= :AND (get-in ast [:left :left :operator])))
    (is (= :NOT (get-in ast [:left :left :left :operator])))
    (is (= :EQEQ (get-in ast [:left :left :left :operand :operator])))))

(deftest parser-builds-function-and-control-flow-ast
  (let [{:keys [ast errors]} (parser/parse
                              "function f(x:int) requires x >= 0 returns r:int ensures r >= 0 uses t:int { t = x + 1; if (t > 1) { r = t; } else r = 0; while (r < 2) invariant r >= 0 { r = r + 1; } assert r >= 0; assume true; }")
        function (first (:functions ast))
        statements (get-in function [:body :statements])]
    (is (empty? errors))
    (is (= "f" (:name function)))
    (is (= "x" (:name (first (:params function)))))
    (is (= "r" (:name (first (:returns function)))))
    (is (some? (:requires function)))
    (is (some? (:ensures function)))
    (is (= "t" (:name (first (:locals function)))))
    (is (= [:assign :if :while :assert :assume] (mapv :node statements)))))

(deftest parser-desugars-array-update-and-parses-tuple-call
  (let [{:keys [ast errors]} (parser/parse
                              "function g(a:int[]) returns x:int, y:int uses b:int[] { b = a; b[0] = 2; x, y = pair(1); }")
        statements (get-in ast [:functions 0 :body :statements])]
    (is (empty? errors))
    (is (= [:assign :array-update :tuple-assign] (mapv :node statements)))
    (is (= "b" (:target (second statements))))
    (is (= 1 (count (:indices (second statements)))))
    (is (= ["x" "y"] (:targets (nth statements 2))))
    (is (= :call (get-in statements [2 :value :node])))))

(deftest parser-reports-missing-delimiters-and-extra-tokens
  (let [{:keys [errors]} (parser/parse "function f() returns r:int { r = 1 } junk")
        empty-errors (:errors (parser/parse ""))
        missing-paren (:errors (parser/parse
                                "function f() returns r:int { if (true { r = 1; } }"))
        invalid-operator (:errors (parser/parse "function f() returns r:int { r = 1 + ; }"))
        invalid-not (:errors (parser/parse "function f() returns r:int { r = 1 + not 2; }"))
        missing-return (:errors (parser/parse "function f() returns ensures true {}"))]
    (is (= 2 (count errors)))
    (is (= "';'" (:expected (first errors))))
    (is (= :RBRACE (:found-type (first errors))))
    (is (= :IDENT (:found-type (second errors))))
    (is (= "function declaration" (:expected (first empty-errors))))
    (is (= "')'" (:expected (first missing-paren))))
    (is (= "expression" (:expected (first invalid-operator))))
    (is (= "expression" (:expected (first invalid-not))))
    (is (= "return variable" (:expected (first missing-return))))))

(deftest parser-recovers-at-block-end-without-looping
  (let [{:keys [ast errors]} (parser/parse
                              "function f() returns r:int { r = 1; r = 2 }")
        statements (get-in ast [:functions 0 :body :statements])]
    (is (= 1 (count errors)))
    (is (= 1 (count statements)))
    (is (= :assign (:node (first statements))))))

(deftest parser-recovers-at-end-of-input
  (let [result (deref (future (parser/parse
                               "function f() returns r:int { r = 1; r = 2"))
                       1000 ::timeout)]
    (is (not= ::timeout result))
    (when-not (= ::timeout result)
      (is (= ["';'" "'}'"] (mapv :expected (:errors result))))
      (is (= 1 (count (get-in result [:ast :functions 0 :body :statements])))))))

(deftest parser-recovers-after-invalid-tuple-assignment
  (doseq [[bad-assignment expected]
          [["x, y = 1;" "function call after tuple assignment"]
           ["x[0], y = pair();" "unindexed tuple assignment targets"]]]
    (let [source (str "function f() returns x:int, y:int, r:int { "
                      bad-assignment " r = 2; }")
          bad-fragment (if (= expected "function call after tuple assignment") "1;" "x[0]")
          error-column (+ 1 (.indexOf ^String source bad-assignment)
                          (.indexOf ^String bad-assignment bad-fragment))
          {:keys [ast errors]} (parser/parse source)
          statements (get-in ast [:functions 0 :body :statements])]
      (is (= 1 (count errors)))
      (is (= expected (:expected (first errors))))
      (is (= error-column (:column (first errors))))
      (is (= :assign (:node (last statements))))
      (is (= "r" (:target (last statements)))))))

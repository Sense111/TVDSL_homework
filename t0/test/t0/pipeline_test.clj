(ns t0.pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [t0.pipeline :as p]))

(defn- strip [v]
  (cond (map? v) (into {} (map (fn [[k x]] [k (strip x)])) (dissoc v :position :target-type :desugared-from))
        (vector? v) (mapv strip v)
        :else v))

(deftest tokens-ast-and-types-for-a-valid-program
  (let [r (p/run (slurp "examples/ok.funny"))]
    (is (empty? (:errors r)))
    (is (= [:FUNCTION :IDENT :LPAREN] (mapv :type (take 3 (:tokens r)))))
    (is (= 3 (count (:functions (:ast r)))))))

(deftest array-update-is-desugared-to-set
  (let [r (p/run "function f(a: int[], i: int) returns b: int[] { b = a; b[i] = 42; }")
        stmt (second (:statements (:body (first (:functions (:desugared r))))))]
    (is (empty? (:errors r)))
    (is (= {:node :assign :target "b"
            :value {:node :call :name "set" :type :int-array
                    :args [{:node :name :name "b" :type :int-array}
                           {:node :name :name "i" :type :int :symbol :param}
                           {:node :int :value 42 :type :int}]}}
           (-> (strip stmt) (update-in [:value :args 2 :value] long))))))

(deftest local-type-is-inferred-before-desugaring
  (let [r (p/run "function f(n: int) returns r: int uses t { t = n + 1; r = t; }")
        local (first (:locals (first (:functions (:desugared r)))))]
    (is (empty? (:errors r)))
    (is (= {:name "t" :type :int :inferred? true} (select-keys local [:name :type :inferred?])))))

(deftest negative-syntax-and-type-scenarios
  (is (seq (:errors (p/run ""))))
  (is (= :syntax (:phase (first (:errors (p/run "function f( returns r: int { r = 1; }"))))))
  (is (= :syntax (:phase (first (:errors (p/run "function f() returns r: int { r = 1 }"))))))
  (is (= :read-only (:kind (first (:errors (p/run "function f(a: int) returns r: int { a = 1; r = a; }")))))))

(deftest lexical-error-yields-no-tokens
  (is (nil? (p/tokens "01"))))

(deftest no-desugaring-and-no-false-errors-after-syntax-error
  (let [r (p/run "function f() returns r: int uses t { t = ; r = t; }")]
    (is (= [:syntax] (mapv :phase (:errors r))))
    (is (nil? (:desugared r)))))

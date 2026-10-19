(ns hw1.regex-test
  (:require [clojure.test :refer [deftest is]]
            [hw1.regex :as regex]
            [hw1.automata :as automata]))

(deftest regex-parses-classes-escapes-and-operators
  (is (= [:concat [[:char \a]
                   [:plus [:alt [[:char \b] [:char \c]]]]
                   [:optional [:class #{\A \B \C \+}]]]]
         (regex/parse "a(b|c)+[A-C\\+]?"))))

(deftest thompson-nfa-accepts-epsilon-and-repetition
  (let [nfa (automata/compile-nfa (regex/parse "a(b|c)*d"))
        empty-repeat (automata/compile-nfa (regex/parse "a*"))]
    (is (automata/nfa-matches? nfa "ad"))
    (is (automata/nfa-matches? nfa "abcbcd"))
    (is (not (automata/nfa-matches? nfa "abce")))
    (is (automata/nfa-matches? empty-repeat ""))
    (is (automata/nfa-matches? empty-repeat "aaa"))))

(deftest regex-supports-complement-classes-and-rejects-malformed-patterns
  (let [nfa (automata/compile-nfa (regex/parse "[^a]"))]
    (is (automata/nfa-matches? nfa "b"))
    (is (not (automata/nfa-matches? nfa "a"))))
  (doseq [pattern ["(" "[a" "\\"]]
    (is (thrown? clojure.lang.ExceptionInfo (regex/parse pattern)))))

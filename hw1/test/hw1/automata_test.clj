(ns hw1.automata-test
  (:require [clojure.test :refer [deftest is]]
            [hw1.regex :as regex]
            [hw1.automata :as automata]))

(defn- run-dfa [dfa input]
  (let [state (reduce (fn [state ch]
                        (get-in dfa [:transitions state ch]))
                      (:start dfa)
                      input)]
    (get-in dfa [:accepts state])))

(deftest subset-construction-recognizes-union
  (let [nfa (automata/combine-nfas
             [{:token :A :priority 0 :nfa (automata/compile-nfa (regex/parse "a"))}
              {:token :AB :priority 1 :nfa (automata/compile-nfa (regex/parse "ab"))}])
        dfa (automata/determinize nfa #{\a \b})]
    (is (= :A (run-dfa dfa "a")))
    (is (= :AB (run-dfa dfa "ab")))
    (is (nil? (run-dfa dfa "b")))))

(deftest hopcroft-merges-equivalent-states-and-keeps-labels
  (let [dfa {:start 0
             :trap 3
             :alphabet #{\a \b}
             :transitions {0 {\a 1 \b 2}
                           1 {\a 1 \b 3}
                           2 {\a 1 \b 2}
                           3 {\a 3 \b 3}}
             :accepts {1 :A}}
        minimized (automata/minimize dfa)]
    (is (= 3 (count (:transitions minimized))))
    (is (= :A (run-dfa minimized "a")))
    (is (nil? (run-dfa minimized "b")))
    (is (= 3 (count (automata/table minimized))))))

(deftest table-uses-ascii-code-columns
  (let [rows (automata/table {:start 0
                              :alphabet #{\a \b}
                              :transitions {0 {\a 1 \b 0}}
                              :accepts {1 :A}})]
    (is (= #{97 98} (set (keys (:transitions (first rows))))))))

(deftest minimize-creates-a-trap-when-none-is-supplied
  (let [dfa {:start 0
             :alphabet #{\a}
             :transitions {0 {\a 0}}
             :accepts {0 :A}}
        minimized (automata/minimize dfa)]
    (is (some? (:trap minimized)))
    (is (contains? (:transitions minimized) (:trap minimized)))
    (is (= (:trap minimized)
           (get-in minimized [:transitions (:trap minimized) \a])))))

(deftest minimize-handles-an-unreachable-empty-subset
  (let [nfa (automata/compile-nfa (regex/parse "a*"))
        dfa (automata/determinize nfa #{\a})
        minimized (automata/minimize dfa)]
    (is (nil? (:trap dfa)))
    (is (some? (:trap minimized)))
    (is (= (:trap minimized)
           (get-in minimized [:transitions (:trap minimized) \a])))))

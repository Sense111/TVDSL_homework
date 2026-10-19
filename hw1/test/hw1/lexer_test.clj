(ns hw1.lexer-test
  (:require [clojure.test :refer [deftest is]]
            [hw1.automata :as automata]
            [hw1.lexer :as lexer]))

(defonce test-lexer (delay (lexer/compile-lexer)))

(deftest scanner-recognizes-keywords-and-longest-identifiers
  (let [tokens (lexer/scan @test-lexer "if ifx functionx _item9")]
    (is (= [:IF :IDENT :IDENT :IDENT] (mapv :type tokens)))
    (is (= ["if" "ifx" "functionx" "_item9"] (mapv :text tokens)))
    (is (= [:FUNCTION :RETURNS :WHILE :IF :ELSE :ASSERT :ASSUME :INVARIANT :LENGTH]
           (mapv :type (lexer/scan @test-lexer
                                  "function returns while if else assert assume invariant length"))))))

(deftest scanner-skips-whitespace-and-comments
  (is (empty? (lexer/scan @test-lexer "")))
  (is (empty? (lexer/scan @test-lexer " \t\r\n // comment\n")))
  (is (= [{:type :IDENT :text "name" :line 2 :column 1}]
         (lexer/scan @test-lexer "// comment\nname"))))

(deftest scanner-counts-crlf-and-cr-comment-lines-once
  (doseq [newline ["\r\n" "\r"]]
    (is (= [{:type :IDENT :text "name" :line 2 :column 1}]
           (lexer/scan @test-lexer (str "// comment" newline "name"))))))

(deftest scanner-rejects-leading-zero-and-nonascii
  (let [leading-zero (try
                       (lexer/scan @test-lexer "01")
                       nil
                       (catch clojure.lang.ExceptionInfo e e))
        double-zero (try
                      (lexer/scan @test-lexer "00")
                      nil
                      (catch clojure.lang.ExceptionInfo e e))
        nonascii (try
                   (lexer/scan @test-lexer "xλ")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
    (is (re-find #"leading zero" (.getMessage leading-zero)))
    (is (re-find #"leading zero" (.getMessage double-zero)))
    (is (= {:line 1 :column 2} (select-keys (ex-data leading-zero) [:line :column])))
    (is (= {:line 1 :column 2} (select-keys (ex-data nonascii) [:line :column])))))

(deftest scanner-covers-required-operators
  (let [text "()[]{},;: + - * / == != <= >= < > = ->"
        tokens (lexer/scan @test-lexer text)]
    (is (= ["(" ")" "[" "]" "{" "}" "," ";" ":"
            "+" "-" "*" "/" "==" "!=" "<=" ">=" "<" ">" "=" "->"]
           (mapv :text tokens)))
    (is (= :EQEQ (:type (nth tokens 13))))))

(deftest exported-table-has-ordered-columns-for-all-ascii
  (let [row (first (automata/table @test-lexer))]
    (is (= (vec (range 128)) (vec (keys (:transitions row)))))))

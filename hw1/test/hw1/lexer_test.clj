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
  (let [row (first (:states (automata/export @test-lexer)))]
    (is (= (vec (range 128)) (vec (keys (:transitions row)))))))

(deftest export-is-self-contained
  (let [exported (automata/export @test-lexer)
        read-back (read-string (pr-str exported))]
    (is (= (:start @test-lexer) (:start read-back)))
    (is (= (:trap @test-lexer) (:trap read-back)))
    (is (= #{:WS :COMMENT} (:skip-types read-back)))
    (is (= (:state-counts @test-lexer) (:state-counts read-back)))
    (is (= (count (:transitions @test-lexer)) (count (:states read-back))))))

;; Ожидания для каждого ASCII-кода, записанные вручную, без помощи лексера.
(def ^:private expected-single-char
  (merge
   (into {} (map (fn [c] [(int c) [:IDENT]]) (concat (range (int \a) (inc (int \z)))
                                                    (range (int \A) (inc (int \Z)))
                                                    [(int \_)])))
   (into {} (map (fn [c] [c [:INT]]) (range (int \0) (inc (int \9)))))
   (into {} (map (fn [c] [c []]) [9 10 13 32]))
   {(int \() [:LPAREN] (int \)) [:RPAREN] (int \[) [:LBRACKET] (int \]) [:RBRACKET]
    (int \{) [:LBRACE] (int \}) [:RBRACE] (int \,) [:COMMA] (int \;) [:SEMI]
    (int \:) [:COLON] (int \+) [:PLUS] (int \-) [:MINUS] (int \*) [:STAR]
    (int \/) [:SLASH] (int \<) [:LT] (int \>) [:GT] (int \=) [:EQ]}))

(deftest scanner-handles-every-single-ascii-character
  (doseq [code (range 128)]
    (let [source (str (char code))
          expected (get expected-single-char code :error)
          actual (try (mapv :type (lexer/scan @test-lexer source))
                      (catch clojure.lang.ExceptionInfo e
                        (is (= {:line 1 :column 1} (select-keys (ex-data e) [:line :column]))
                            (str "error position for code " code))
                        :error))]
      (is (= expected actual) (str "ASCII code " code)))))

(deftest scanner-handles-ascii-characters-in-context
  (doseq [code (range 128)
          :let [expected (get expected-single-char code :error)]
          :when (not= expected :error)
          :let [source (str "x " (char code) " y")
                types (mapv :type (lexer/scan @test-lexer source))]]
    (is (= (vec (concat [:IDENT] expected [:IDENT])) types) (str "ASCII code " code)))
  (doseq [code (range 128)
          :when (= :error (get expected-single-char code :error))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (lexer/scan @test-lexer (str "x " (char code) " y")))
        (str "ASCII code " code))))

(deftest scanner-keeps-compound-operators-and-comments
  (is (= [:EQEQ :NE :LE :GE :IMPLIES] (mapv :type (lexer/scan @test-lexer "== != <= >= ->"))))
  (is (= [:EQ :EQ] (mapv :type (lexer/scan @test-lexer "= ="))))
  (is (= [:IDENT :IDENT] (mapv :type (lexer/scan @test-lexer "a // $#@!\u0001 \nb")))))

(deftest non-ascii-text-is-allowed-inside-comments-only
  (is (= [{:type :IDENT :text "name" :line 2 :column 1}]
         (lexer/scan @test-lexer "// комментарий λ\nname")))
  (is (= [{:type :IDENT :text "a" :line 1 :column 1}]
         (lexer/scan @test-lexer "a // без перевода строки")))
  (is (= [:IDENT :IDENT]
         (mapv :type (lexer/scan @test-lexer "a // ок\r\nb"))))
  (is (thrown? clojure.lang.ExceptionInfo (lexer/scan @test-lexer "a λ // ок"))))

(ns hw1.lexer
  (:require [hw1.automata :as automata]
            [hw1.regex :as regex]))

(def ^:private ascii (set (map char (range 128))))
(def ^:private regex-metacharacters #{\\ \. \^ \$ \| \? \* \+ \( \) \[ \] \{ \}})

(defn- literal-pattern [text]
  (apply str (map (fn [ch]
                    (if (contains? regex-metacharacters ch)
                      (str \\ ch)
                      ch))
                  text)))

(def ^:private keywords
  [["function" :FUNCTION] ["returns" :RETURNS] ["uses" :USES]
   ["requires" :REQUIRES] ["ensures" :ENSURES] ["while" :WHILE]
   ["if" :IF] ["else" :ELSE] ["true" :TRUE] ["false" :FALSE]
   ["assert" :ASSERT] ["assume" :ASSUME] ["invariant" :INVARIANT]
   ["length" :LENGTH] ["int" :INT_TYPE] ["not" :NOT]
   ["and" :AND] ["or" :OR]])

(def ^:private operators
  [["==" :EQEQ] ["!=" :NE] ["<=" :LE] [">=" :GE] ["->" :IMPLIES]
   ["+" :PLUS] ["-" :MINUS] ["*" :STAR] ["/" :SLASH]
   ["<" :LT] [">" :GT] ["=" :EQ] ["," :COMMA] [";" :SEMI] [":" :COLON]
   ["(" :LPAREN] [")" :RPAREN] ["[" :LBRACKET] ["]" :RBRACKET]
   ["{" :LBRACE] ["}" :RBRACE]])

(def ^:private token-specs
  (vec (concat
        [{:token :WS :pattern "[ \\t\\r\\n]+" :skip? true}
         {:token :COMMENT :pattern "//[^\\r\\n]*" :skip? true}
         {:token :INT :pattern "0|[1-9][0-9]*"}]
        (map (fn [[text token]] {:token token :pattern (literal-pattern text)}) keywords)
        [{:token :IDENT :pattern "[A-Za-z_][A-Za-z0-9_]*"}]
        (map (fn [[text token]] {:token token :pattern (literal-pattern text)}) operators))))

(defn compile-lexer
  "Compile and minimize the combined Funny token automaton."
  []
  (let [entries (mapv (fn [priority {:keys [token pattern]}]
                        {:token token
                         :priority priority
                         :nfa (automata/compile-nfa (regex/parse pattern))})
                      (range)
                      token-specs)
        nfa (automata/combine-nfas entries)
        dfa (automata/determinize nfa ascii)
        minimized (automata/minimize dfa)]
    (assoc minimized
           :state-counts {:before (count (:transitions dfa))
                          :after (count (:transitions minimized))}
           :skip-types (set (map :token (filter :skip? token-specs))))))

(defn- advance-position [text line column]
  (loop [index 0, line line, column column]
    (if (= index (count text))
      [line column]
      (let [ch (.charAt ^String text index)
            next-ch (when (< (inc index) (count text)) (.charAt ^String text (inc index)))]
        (cond
          (= ch \newline) (recur (inc index) (inc line) 1)
          (= ch \return) (if (= next-ch \newline)
                            (recur (inc index) line (inc column))
                            (recur (inc index) (inc line) 1))
          :else (recur (inc index) line (inc column)))))))

(defn- lexical-error [message source index line column]
  (let [found (when (< index (count source)) (subs source index (inc index)))]
    (throw (ex-info (str message " at line " line ", column " column
                          (when found (str ": " (pr-str found))))
                    {:line line :column column :index index :found found}))))

(defn scan
  "Scan source using maximal munch and return positioned non-skip tokens."
  [lexer source]
  (let [source (str source)
        size (count source)
        start-state (:start lexer)
        trap (:trap lexer)
        skip-types (:skip-types lexer)]
    (loop [index 0, line 1, column 1, tokens []]
      (if (= index size)
        tokens
        (let [match
              (loop [state start-state, cursor index, last-match nil]
                (if (>= cursor size)
                  last-match
                  (let [ch (.charAt ^String source cursor)]
                    (if (not (contains? ascii ch))
                      last-match
                      (let [next-state (get-in lexer [:transitions state ch])]
                        (if (or (nil? next-state) (= next-state trap))
                          last-match
                          (let [next-cursor (inc cursor)
                                token (get-in lexer [:accepts next-state])]
                            (recur next-state next-cursor
                                   (if token [next-cursor token] last-match))))))))) ]
          (when-not match
            (lexical-error "Unrecognized input" source index line column))
          (let [[end token-type] match
                text (subs source index end)
                next-char (when (< end size) (.charAt ^String source end))]
            (when (and (= token-type :INT) (= text "0") next-char
                       (Character/isDigit ^char next-char))
              (lexical-error "Invalid leading zero in integer" source end line (inc column)))
            (let [[next-line next-column] (advance-position text line column)
                  token {:type token-type :text text :line line :column column}]
              (recur end next-line next-column
                     (if (contains? skip-types token-type) tokens (conj tokens token))))))))))

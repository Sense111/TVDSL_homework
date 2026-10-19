(ns hw2.parser
  (:require [hw1.lexer :as lexer]))

(def ^:private eof-token {:type :EOF :text ""})
(defonce ^:private compiled-lexer (delay (lexer/compile-lexer)))

(defn- source-end-position [source]
  (loop [index 0, line 1, column 1]
    (if (= index (count source))
      {:line line :column column}
      (let [ch (.charAt ^String source index)
            next-ch (when (< (inc index) (count source)) (.charAt ^String source (inc index)))]
        (cond
          (= ch \newline) (recur (inc index) (inc line) 1)
          (= ch \return) (if (= next-ch \newline)
                            (recur (inc index) line (inc column))
                            (recur (inc index) (inc line) 1))
          :else (recur (inc index) line (inc column)))))))

(defn- context [tokens end-position]
  {:tokens (vec tokens) :end-position end-position :index (atom 0)})

(defn- current-token [ctx]
  (or (get (:tokens ctx) @(:index ctx))
      (merge eof-token (:end-position ctx))))

(defn- advance! [ctx]
  (let [token (current-token ctx)]
    (when-not (= :EOF (:type token))
      (swap! (:index ctx) inc))
    token))

(defn- match! [ctx type]
  (when (= type (:type (current-token ctx)))
    (advance! ctx)))

(defn- parse-error! [ctx expected]
  (let [{:keys [type text line column]} (current-token ctx)
        found (if (= type :EOF) "end of input" (str (:text (current-token ctx)) " (" (name type) ")"))]
    (throw (ex-info (str "Expected " expected ", found " found
                         " at line " line ", column " column)
                    {:line line :column column :expected expected
                     :found (when-not (= type :EOF) text) :found-type type}))))

(defn- located-error! [position expected found found-type]
  (let [{:keys [line column]} position]
    (throw (ex-info (str "Expected " expected ", found " found
                         " at line " line ", column " column)
                    {:line line :column column :expected expected
                     :found found :found-type found-type}))))

(defn- expect! [ctx type description]
  (or (match! ctx type) (parse-error! ctx description)))

(defn- node [tag position fields]
  (merge {:node tag :position (select-keys position [:line :column])} fields))

(defn- parse-tokens-expression [ctx]
  (letfn [(parse-arguments []
            (expect! ctx :LPAREN "'('")
            (if (match! ctx :RPAREN)
              []
              (loop [args [(parse-implication)]]
                (if (match! ctx :COMMA)
                  (recur (conj args (parse-implication)))
                  (do (expect! ctx :RPAREN "')'") args)))))
          (parse-postfix [base]
            (if-let [bracket (match! ctx :LBRACKET)]
              (let [index (parse-implication)]
                (expect! ctx :RBRACKET "']'")
                (recur (node :index bracket {:target base :index index})))
              base))
          (parse-primary []
            (let [{:keys [type text] :as token} (current-token ctx)]
              (case type
                :INT (do (advance! ctx)
                         (node :int token {:value (bigint text)}))
                :TRUE (do (advance! ctx) (node :bool token {:value true}))
                :FALSE (do (advance! ctx) (node :bool token {:value false}))
                :LPAREN (do
                          (advance! ctx)
                          (let [expression (parse-implication)]
                            (expect! ctx :RPAREN "')'")
                            expression))
                :IDENT (do
                         (advance! ctx)
                         (parse-postfix
                          (if (= :LPAREN (:type (current-token ctx)))
                            (node :call token {:name text :args (parse-arguments)})
                            (node :name token {:name text}))))
                :LENGTH (do
                          (advance! ctx)
                          (parse-postfix
                           (if (= :LPAREN (:type (current-token ctx)))
                             (node :call token {:name text :args (parse-arguments)})
                             (node :name token {:name text}))))
                (parse-error! ctx "expression"))))
          (parse-unary []
            (if-let [operator (match! ctx :MINUS)]
              (node :unary operator
                    {:operator (:type operator) :operand (parse-unary)})
              (parse-primary)))
          (parse-multiplicative []
            (loop [left (parse-unary)]
              (if (contains? #{:STAR :SLASH} (:type (current-token ctx)))
                (let [operator (advance! ctx)
                      right (parse-unary)]
                  (recur (node :binary operator
                               {:operator (:type operator) :left left :right right})))
                left)))
          (parse-additive []
            (loop [left (parse-multiplicative)]
              (if (contains? #{:PLUS :MINUS} (:type (current-token ctx)))
                (let [operator (advance! ctx)
                      right (parse-multiplicative)]
                  (recur (node :binary operator
                               {:operator (:type operator) :left left :right right})))
                left)))
          (parse-comparison []
            (let [left (parse-additive)]
              (if (contains? #{:EQEQ :NE :LE :GE :LT :GT} (:type (current-token ctx)))
                (let [operator (advance! ctx)
                      right (parse-additive)]
                  (node :binary operator
                        {:operator (:type operator) :left left :right right}))
                left)))
          (parse-not []
            (if-let [operator (match! ctx :NOT)]
              (node :unary operator {:operator :NOT :operand (parse-not)})
              (parse-comparison)))
          (parse-and []
            (loop [left (parse-not)]
              (if-let [operator (match! ctx :AND)]
                (recur (node :binary operator
                             {:operator :AND :left left :right (parse-not)}))
                left)))
          (parse-or []
            (loop [left (parse-and)]
              (if-let [operator (match! ctx :OR)]
                (recur (node :binary operator
                             {:operator :OR :left left :right (parse-and)}))
                left)))
          (parse-implication []
            (let [left (parse-or)]
              (if-let [operator (match! ctx :IMPLIES)]
                (node :binary operator
                      {:operator :IMPLIES :left left :right (parse-implication)})
                left)))]
    (parse-implication)))

(defn parse-expression
  "Parse one Funny expression and return its AST or a positioned error."
  [source]
  (try
    (let [source (str source)
          tokens (lexer/scan @compiled-lexer source)
          ctx (context tokens (source-end-position source))
          ast (parse-tokens-expression ctx)]
      (when-not (= :EOF (:type (current-token ctx)))
        (parse-error! ctx "end of expression"))
      {:ast ast :errors []})
    (catch clojure.lang.ExceptionInfo error
      (let [data (ex-data error)]
        {:ast nil
         :errors [(merge {:message (.getMessage error)}
                         (select-keys data [:line :column :expected :found :found-type]))]}))))

(defn- error-map [error]
  (merge {:message (.getMessage error)}
         (select-keys (ex-data error) [:line :column :expected :found :found-type])))

(defn- record-error! [ctx error]
  (swap! (:errors ctx) conj (error-map error)))

(defn- recover-statement! [ctx before]
  (loop []
    (when-not (contains? #{:EOF :RBRACE :SEMI} (:type (current-token ctx)))
      (advance! ctx)
      (recur)))
  (when (= :SEMI (:type (current-token ctx)))
    (advance! ctx))
  (when (and (= before @(:index ctx))
             (not (contains? #{:EOF :RBRACE} (:type (current-token ctx)))))
    (advance! ctx)))

(defn- recover-function! [ctx before]
  (when (= before @(:index ctx))
    (advance! ctx))
  (loop []
    (when-not (contains? #{:EOF :FUNCTION} (:type (current-token ctx)))
      (advance! ctx)
      (recur))))

(defn- parse-program-tokens [ctx]
  (letfn [(parse-type []
            (expect! ctx :INT_TYPE "type 'int'")
            (if (match! ctx :LBRACKET)
              (do (expect! ctx :RBRACKET "']'") :int-array)
              :int))
          (parse-variable []
            (let [name-token (expect! ctx :IDENT "identifier")]
              (expect! ctx :COLON "':'")
              {:name (:text name-token) :type (parse-type)
               :position (select-keys name-token [:line :column])}))
          (parse-local []
            (let [name-token (expect! ctx :IDENT "identifier")]
              (if (match! ctx :COLON)
                {:name (:text name-token) :type (parse-type)
                 :position (select-keys name-token [:line :column])}
                {:name (:text name-token) :type nil
                 :position (select-keys name-token [:line :column])})))
          (parse-comma-list [parse-one closing-type]
            (if (= closing-type (:type (current-token ctx)))
              []
              (loop [items [(parse-one)]]
                (if (match! ctx :COMMA)
                  (recur (conj items (parse-one)))
                  items))))
          (parse-block [open-token]
            (loop [statements []]
              (case (:type (current-token ctx))
                :RBRACE (do (advance! ctx)
                            (node :block open-token {:statements statements}))
                :EOF (do (record-error! ctx
                                        (try (parse-error! ctx "'}'")
                                             (catch clojure.lang.ExceptionInfo error error)))
                         (node :block open-token {:statements statements}))
                (let [before @(:index ctx)
                      result (try {:statement (parse-statement)}
                                  (catch clojure.lang.ExceptionInfo error {:error error}))]
                  (if-let [error (:error result)]
                    (do
                      (record-error! ctx error)
                      (recover-statement! ctx before)
                      (recur statements))
                    (recur (conj statements (:statement result))))))))
          (parse-lvalue []
            (let [name-token (expect! ctx :IDENT "assignment target")
                  base (node :name name-token {:name (:text name-token)})]
              (loop [indices []]
                (if-let [bracket (match! ctx :LBRACKET)]
                  (let [index (parse-tokens-expression ctx)]
                    (expect! ctx :RBRACKET "']'")
                    (recur (conj indices index)))
                  {:name (:text name-token) :base base :indices indices :token name-token}))))
          (parse-assignment []
            (let [targets (loop [targets [(parse-lvalue)]]
                            (if (match! ctx :COMMA)
                              (recur (conj targets (parse-lvalue)))
                              targets))
                  equals (expect! ctx :EQ "'='")
                  value (parse-tokens-expression ctx)]
              (cond
                (> (count targets) 1)
                (do
                  (when-let [target (first (filter #(seq (:indices %)) targets))]
                    (located-error! (:token target)
                                    "unindexed tuple assignment targets"
                                    "indexed assignment target" :IDENT))
                  (when-not (= :call (:node value))
                    (located-error! (:position value)
                                    "function call after tuple assignment"
                                    (str (name (:node value)) " expression")
                                    (case (:node value)
                                      :int :INT
                                      :bool (if (:value value) :TRUE :FALSE)
                                      :name :IDENT
                                      :binary (:operator value)
                                      :unary (:operator value)
                                      :call :IDENT
                                      nil)))
                  (expect! ctx :SEMI "';'")
                  (node :tuple-assign equals
                        {:targets (mapv :name targets) :value value}))

                (seq (:indices (first targets)))
                (do
                  (expect! ctx :SEMI "';'")
                  (node :array-update (:token (first targets))
                        {:target (:name (first targets))
                         :indices (:indices (first targets)) :value value}))

                :else
                (do
                  (expect! ctx :SEMI "';'")
                  (node :assign (:token (first targets))
                        {:target (:name (first targets)) :value value})))))
          (parse-if []
            (let [if-token (advance! ctx)]
              (expect! ctx :LPAREN "'('")
              (let [condition (parse-tokens-expression ctx)]
                (expect! ctx :RPAREN "')'")
                (let [then-branch (parse-statement)
                      else-branch (when (match! ctx :ELSE) (parse-statement))]
                  (cond-> (node :if if-token {:condition condition :then then-branch})
                    else-branch (assoc :else else-branch))))))
          (parse-while []
            (let [while-token (advance! ctx)]
              (expect! ctx :LPAREN "'('")
              (let [condition (parse-tokens-expression ctx)]
                (expect! ctx :RPAREN "')'")
                (let [invariant (when (match! ctx :INVARIANT)
                                  (parse-tokens-expression ctx))
                      body (parse-statement)]
                  (cond-> (node :while while-token {:condition condition :body body})
                    invariant (assoc :invariant invariant))))))
          (parse-statement []
            (case (:type (current-token ctx))
              :LBRACE (parse-block (advance! ctx))
              :IF (parse-if)
              :WHILE (parse-while)
              :ASSERT (let [token (advance! ctx)
                            predicate (parse-tokens-expression ctx)]
                        (expect! ctx :SEMI "';'")
                        (node :assert token {:predicate predicate}))
              :ASSUME (let [token (advance! ctx)
                            predicate (parse-tokens-expression ctx)]
                        (expect! ctx :SEMI "';'")
                        (node :assume token {:predicate predicate}))
              :IDENT (parse-assignment)
              (parse-error! ctx "statement")))
          (parse-function []
            (let [function-token (expect! ctx :FUNCTION "'function'")
                  name-token (expect! ctx :IDENT "function name")]
              (expect! ctx :LPAREN "'('")
              (let [params (parse-comma-list parse-variable :RPAREN)]
                (expect! ctx :RPAREN "')'")
                (let [requires (when (match! ctx :REQUIRES)
                                 (parse-tokens-expression ctx))]
                  (expect! ctx :RETURNS "'returns'")
                  (let [returns (let [values (parse-comma-list parse-variable :ENSURES)]
                                  (when (empty? values)
                                    (parse-error! ctx "return variable"))
                                  values)
                        ensures (when (match! ctx :ENSURES)
                                  (parse-tokens-expression ctx))
                        locals (when (match! ctx :USES)
                                 (loop [items [(parse-local)]]
                                   (if (match! ctx :COMMA)
                                     (recur (conj items (parse-local)))
                                     items)))
                        body (parse-statement)]
                    (cond-> (node :function function-token
                                  {:name (:text name-token)
                                   :params params :returns returns :body body})
                      requires (assoc :requires requires)
                      ensures (assoc :ensures ensures)
                      locals (assoc :locals locals)))))))
          (parse-module []
            (loop [functions []]
              (case (:type (current-token ctx))
                :EOF (if (and (empty? functions) (empty? (:tokens ctx)))
                       (do (record-error! ctx
                                          (try (parse-error! ctx "function declaration")
                                               (catch clojure.lang.ExceptionInfo error error)))
                           (node :module (current-token ctx) {:functions functions}))
                       (node :module (or (first (:tokens ctx)) (current-token ctx))
                             {:functions functions}))
                :FUNCTION
                (let [before @(:index ctx)
                      result (try {:function (parse-function)}
                                  (catch clojure.lang.ExceptionInfo error {:error error}))]
                  (if-let [error (:error result)]
                    (do (record-error! ctx error)
                        (recover-function! ctx before)
                        (recur functions))
                    (recur (conj functions (:function result)))))
                (let [before @(:index ctx)
                      error (try (parse-error! ctx "function declaration")
                                 (catch clojure.lang.ExceptionInfo error error))]
                  (record-error! ctx error)
                  (when (= before @(:index ctx)) (advance! ctx))
                  (recur functions)))))]
    (parse-module)))

(defn parse
  "Parse a Funny program into a module AST and collect recoverable syntax errors."
  [source]
  (let [source (str source)]
    (try
      (let [tokens (lexer/scan @compiled-lexer source)
            ctx (assoc (context tokens (source-end-position source)) :errors (atom []))
            ast (parse-program-tokens ctx)]
        {:ast ast :errors @(:errors ctx)})
      (catch clojure.lang.ExceptionInfo error
        {:ast nil :errors [(error-map error)]}))))

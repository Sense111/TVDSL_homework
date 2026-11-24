(ns hw3.types
  "Разрешение имён и проверка типов для AST Funny, построенного в HW2.

  `check` возвращает {:ast <аннотированный модуль> :errors [...]}.
  Каждое выражение в AST получает `:type` (:int, :int-array, :bool или :error),
  имена - ещё и `:symbol` (:param/:return/:local), а функции - `:signature`
  и `:locals` с уже определёнными типами."
  (:require [clojure.string :as str]))

(def ^:private max-int 2147483647)

(def builtins
  {"length" {:params [:int-array] :returns [:int]}})

(defn type-name [t]
  (case t
    :int "int" :int-array "int[]" :bool "bool" :error "<error>" nil "<unknown>"
    (str t)))

(defn- report!
  ([ctx position message kind]
   (swap! (:errors ctx) conj
          {:phase :semantic :kind kind :message message
           :line (:line position) :column (:column position)})
   nil))

(defn- at [position] (str " at line " (:line position) ", column " (:column position)))

;; символы

(defn- declare! [ctx name entry]
  (if-let [old (get @(:env ctx) name)]
    (report! ctx (:position entry)
             (str "Duplicate declaration of '" name "'; first declared" (at (:position old)))
             :duplicate-declaration)
    (swap! (:env ctx) assoc name entry)))

(def ^:private visible-kinds
  {:requires #{:param}
   :ensures #{:param :return}
   :body #{:param :return :local}})

(defn- lookup [ctx scope name position]
  (if-let [entry (get @(:env ctx) name)]
    (if (contains? (visible-kinds scope) (:kind entry))
      entry
      (report! ctx position
               (str "'" name "' (" (clojure.core/name (:kind entry)) ") is not visible in "
                    (clojure.core/name scope) "; allowed: "
                    (str/join ", " (map clojure.core/name (sort (visible-kinds scope)))))
               :not-visible))
    (report! ctx position (str "Unknown name '" name "'") :unknown-name)))

;; выражения

(declare check-expr)

(defn- check-call
  "Проверяет вызов функции. Возвращает пару [узел типы-результатов]."
  [ctx scope expr]
  (let [{:keys [name args position]} expr
        args' (mapv #(check-expr ctx scope %) args)
        sig (or (get builtins name) (get (:sigs ctx) name))
        node (assoc expr :args args')]
    (if-not sig
      (do (report! ctx position (str "Unknown function '" name "'") :unknown-function)
          [(assoc node :type :error) nil])
      (do
        (when (not= (count args) (count (:params sig)))
          (report! ctx position
                   (str "Function '" name "' expects " (count (:params sig))
                        " argument(s), got " (count args))
                   :arity))
        (when (= (count args) (count (:params sig)))
          (doseq [[i expected arg] (map vector (iterate inc 1) (:params sig) args')
                  :let [actual (:type arg)]
                  :when (and (not= actual :error) (not= actual expected))]
            (report! ctx (:position arg)
                     (str "Argument " i " of '" name "' must be " (type-name expected)
                          ", got " (type-name actual))
                     :type-mismatch)))
        [(assoc node :signature (dissoc sig :function)) (:returns sig)]))))

(def ^:private arithmetic #{:PLUS :MINUS :STAR :SLASH})
(def ^:private logical #{:AND :OR :IMPLIES})

(defn- operator-text [op]
  (get {:PLUS "+" :MINUS "-" :STAR "*" :SLASH "/" :EQEQ "==" :NE "!=" :LE "<="
        :GE ">=" :LT "<" :GT ">" :AND "and" :OR "or" :IMPLIES "->" :NOT "not"} op (str op)))

(defn- expect-operands! [ctx op expected position operands]
  (let [types (map :type operands)]
    (when (and (not-any? #{:error} types) (some #(not= expected %) types))
      (report! ctx position
               (str "Operator '" (operator-text op) "' expects " (type-name expected)
                    " operands, got " (str/join " and " (map type-name types)))
               :type-mismatch))))

(defn- check-index! [ctx index]
  (when-not (contains? #{:int :error} (:type index))
    (report! ctx (:position index)
             (str "Array index must be int, got " (type-name (:type index)))
             :type-mismatch)))

(defn check-expr
  "Возвращает выражение, аннотированное полем :type."
  [ctx scope expr]
  (case (:node expr)
    :int (do (when (and (= :body scope) (> (:value expr) max-int))
               (report! ctx (:position expr)
                        (str "Integer literal " (:value expr) " does not fit in 32-bit int")
                        :out-of-range))
             (assoc expr :type :int))
    :bool (assoc expr :type :bool)

    :name
    (let [entry (lookup ctx scope (:name expr) (:position expr))]
      (cond
        (nil? entry) (assoc expr :type :error)
        (and (nil? (:type entry)) (:poisoned? entry))
        (assoc expr :type :error :symbol (:kind entry))
        (nil? (:type entry))
        (do (report! ctx (:position expr)
                     (str "Cannot infer the type of local '" (:name expr)
                          "': it is read before its first assignment")
                     :cannot-infer)
            (assoc expr :type :error :symbol (:kind entry)))
        :else (assoc expr :type (:type entry) :symbol (:kind entry))))

    (:unary :binary)
    (let [op (:operator expr)
          ;; -2147483648 - единственный случай, когда литерал больше max-int допустим
          min-literal? (and (= :body scope) (= :unary (:node expr)) (= :MINUS op)
                            (= :int (:node (:operand expr)))
                            (= (inc max-int) (:value (:operand expr))))
          kids (into {} (for [k [:operand :left :right] :when (k expr)]
                          [k (if min-literal?
                               (assoc (k expr) :type :int)
                               (check-expr ctx scope (k expr)))]))
          operand-type (if (or (= :NOT op) (contains? logical op)) :bool :int)]
      (expect-operands! ctx op operand-type (:position expr) (vals kids))
      (merge expr kids {:type (cond (contains? arithmetic op) :int
                                    (= :unary (:node expr)) operand-type
                                    :else :bool)}))

    :index
    (let [target (check-expr ctx scope (:target expr))
          index (check-expr ctx scope (:index expr))]
      (when-not (contains? #{:int-array :error} (:type target))
        (report! ctx (:position expr)
                 (str "Only int[] can be indexed, got " (type-name (:type target)))
                 :type-mismatch))
      (check-index! ctx index)
      (assoc expr :target target :index index
             :type (if (= :int-array (:type target)) :int :error)))

    :call
    (let [[node returns] (check-call ctx scope expr)]
      (cond
        (nil? returns) node
        (= 1 (count returns)) (assoc node :type (first returns))
        :else (do (report! ctx (:position expr)
                           (str "Function '" (:name expr) "' returns " (count returns)
                                " values; a multi-value call is allowed only in a tuple assignment")
                           :type-mismatch)
                  (assoc node :type :error))))))

(defn- check-bool [ctx scope what expr]
  (let [e (check-expr ctx scope expr)]
    (when-not (contains? #{:bool :error} (:type e))
      (report! ctx (:position e) (str what " must be bool, got " (type-name (:type e)))
               :type-mismatch))
    e))

;; операторы

(defn- target-entry
  "Находит переменную, в которую пишем. В параметры писать нельзя.
  Неизвестное имя неявно объявляется локальной (T0: тип выводится из первого присваивания)."
  [ctx name position]
  (if-let [entry (get @(:env ctx) name)]
    (if (= :param (:kind entry))
      (report! ctx position
               (str "Cannot assign to parameter '" name "': parameters are read-only")
               :read-only)
      entry)
    (let [entry {:kind :local :type nil :position position :implicit? true}]
      (swap! (:env ctx) assoc name entry)
      entry)))

(defn- infer!
  "Задаёт тип локальной переменной без аннотации по её первому присваиванию."
  [ctx name t position]
  (swap! (:env ctx) assoc-in [name :assigned?] true)
  (cond
    (contains? #{:int :int-array} t)
    (do (swap! (:env ctx) assoc-in [name :type] t) true)
    (= :error t) false
    :else (do (report! ctx position
                       (str "Cannot infer the type of local '" name "' from a value of type "
                            (type-name t))
                       :cannot-infer)
              false)))

(defn- assign-type!
  "Проверяет, что значение типа t можно положить в `entry`.
  Если это первое присваивание локальной без типа, возвращает :inferred."
  [ctx name entry t position]
  (cond
    ;; значение с ошибкой не задаёт тип: ждём первого корректного присваивания,
    ;; а чтения до него молчат, чтобы не плодить вторичные ошибки
    (= :error t) (do (when (nil? (:type entry))
                       (swap! (:env ctx) update name assoc :assigned? true :poisoned? true))
                     nil)
    (nil? (:type entry)) (when (infer! ctx name t position) :inferred)
    (not= t (:type entry))
    (do (report! ctx position
                 (str "Cannot assign " (type-name t) " to '" name "' of type "
                      (type-name (:type entry)))
                 :type-mismatch)
        nil)))

(defn- check-stmt [ctx stmt]
  (case (:node stmt)
    :block (assoc stmt :statements (mapv #(check-stmt ctx %) (:statements stmt)))

    :assign
    (let [value (check-expr ctx :body (:value stmt))
          entry (target-entry ctx (:target stmt) (:position stmt))
          how (when entry (assign-type! ctx (:target stmt) entry (:type value) (:position value)))
          type (when entry (:type (get @(:env ctx) (:target stmt))))]
      (cond-> (assoc stmt :value value :target-type (or type :error))
        (= how :inferred) (assoc :inferred? true)))

    :array-update
    (let [name (:target stmt)
          entry (target-entry ctx name (:position stmt))
          indices (mapv #(check-expr ctx :body %) (:indices stmt))
          value (check-expr ctx :body (:value stmt))
          how (when entry
                (if (nil? (:type entry))
                  (when (infer! ctx name :int-array (:position stmt)) :inferred)
                  (when-not (contains? #{:int-array :error} (:type entry))
                    (report! ctx (:position stmt)
                             (str "Only int[] can be indexed, '" name "' has type "
                                  (type-name (:type entry)))
                             :type-mismatch))))]
      (run! #(check-index! ctx %) indices)
      (when (> (count indices) 1)
        (report! ctx (:position (second indices))
                 "int[] elements are int and cannot be indexed again (no nested arrays)"
                 :type-mismatch))
      (when-not (contains? #{:int :error} (:type value))
        (report! ctx (:position value)
                 (str "Array element must be int, got " (type-name (:type value)))
                 :type-mismatch))
      (cond-> (assoc stmt :indices indices :value value :target-type :int-array)
        (= how :inferred) (assoc :inferred? true)))

    :tuple-assign
    (let [[call returns] (check-call ctx :body (:value stmt))
          names (:targets stmt)
          positions (or (:target-positions stmt) (repeat (:position stmt)))
          entries (mapv #(target-entry ctx %1 %2) names positions)]
      (when returns
        (if (not= (count returns) (count names))
          (report! ctx (:position stmt)
                   (str "Function '" (:name call) "' returns " (count returns)
                        " value(s), but " (count names) " target(s) given")
                   :arity)
          (doseq [[name entry t position] (map vector names entries returns positions)
                  :when entry]
            (assign-type! ctx name entry t position))))
      (assoc stmt :value (assoc call :type :tuple :return-types returns)))

    :if
    (let [condition (check-bool ctx :body "Condition" (:condition stmt))]
      (cond-> (assoc stmt :condition condition :then (check-stmt ctx (:then stmt)))
        (:else stmt) (assoc :else (check-stmt ctx (:else stmt)))))

    :while
    (let [condition (check-bool ctx :body "Condition" (:condition stmt))
          invariant (when (:invariant stmt) (check-bool ctx :body "Invariant" (:invariant stmt)))]
      (cond-> (assoc stmt :condition condition :body (check-stmt ctx (:body stmt)))
        invariant (assoc :invariant invariant)))

    (:assert :assume)
    (assoc stmt :predicate (check-bool ctx :body (str (name (:node stmt)) " condition") (:predicate stmt)))))

;; функции

(defn- signature [f]
  {:params (mapv :type (:params f)) :returns (mapv :type (:returns f))})

(defn- collect-signatures! [ctx functions]
  (reduce
   (fn [sigs f]
     (cond
       (contains? builtins (:name f))
       (do (report! ctx (:position f) (str "Function name '" (:name f) "' is reserved for a built-in")
                    :duplicate-declaration)
           sigs)
       (contains? sigs (:name f))
       (do (report! ctx (:position f)
                    (str "Duplicate function '" (:name f) "'; first declared"
                         (at (:position (get-in sigs [(:name f) :function]))))
                    :duplicate-declaration)
           sigs)
       :else (assoc sigs (:name f) (assoc (signature f) :function f))))
   {} functions))

(defn- check-function [ctx f]
  (reset! (:env ctx) {})
  (doseq [[kind vars] [[:param (:params f)] [:return (:returns f)] [:local (:locals f)]]
          v vars]
    (declare! ctx (:name v) {:kind kind :type (:type v) :position (:position v)}))
  (let [requires (when (:requires f) (check-bool ctx :requires "requires condition" (:requires f)))
        ensures (when (:ensures f) (check-bool ctx :ensures "ensures condition" (:ensures f)))
        body (check-stmt ctx (:body f))
        locals (mapv (fn [v]
                       (let [entry (get @(:env ctx) (:name v))
                             t (:type entry)]
                         (when (and (nil? t) (not (:assigned? entry))
                                    (= (:position entry) (:position v)))
                           (report! ctx (:position v)
                                    (str "Cannot infer the type of local '" (:name v)
                                         "': it is never assigned")
                                    :cannot-infer))
                         (assoc v :type (or t :error) :inferred? (nil? (:type v)))))
                     (:locals f))
        implicit (->> @(:env ctx)
                      (filter (fn [[_ e]] (:implicit? e)))
                      (sort-by (fn [[_ e]] ((juxt :line :column) (:position e))))
                      (mapv (fn [[n e]]
                              {:name n :position (:position e)
                               :type (or (:type e) :error) :inferred? true})))
        locals (into locals implicit)]
    (cond-> (assoc f :body body :signature (signature f))
      requires (assoc :requires requires)
      ensures (assoc :ensures ensures)
      (seq locals) (assoc :locals locals))))

(defn check
  "Проверяет типы в AST модуля, полученном из hw2.parser/parse."
  [module]
  (let [ctx {:errors (atom []) :env (atom {})}
        sigs (collect-signatures! ctx (:functions module))
        ctx (assoc ctx :sigs sigs)
        functions (mapv #(check-function ctx %) (:functions module))]
    {:ast (assoc module :functions functions)
     :errors (vec (sort-by (juxt :line :column) @(:errors ctx)))}))

(defn check-source
  "Разбирает (HW2) и проверяет типы исходного текста. Синтаксические ошибки добавляются в :errors."
  [source]
  (let [parse (requiring-resolve 'hw2.parser/parse)
        {:keys [ast errors]} (parse source)
        syntax (mapv #(assoc % :phase :syntax) errors)]
    (if ast
      (let [result (check ast)
            semantic (cond->> (:errors result)
                       (seq syntax) (remove #(= :cannot-infer (:kind %))))]
        (assoc result :errors (vec (sort-by (juxt :line :column) (concat syntax semantic)))))
      {:ast nil :errors syntax})))

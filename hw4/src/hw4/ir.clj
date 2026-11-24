(ns hw4.ir
  "Формат IR (см. IR.md): общие функции над инструкциями и генерация IR из
  аннотированного AST (HW3)."
  (:require [clojure.string :as str]))


(defn wrap32
  "Приводит целое к 32-битному знаковому (переполнение по модулю 2^32)."
  [n]
  (long (unchecked-int (long n))))

(defn const? [operand] (integer? operand))
(defn variable? [operand] (string? operand))

(defn terminator? [instr]
  (contains? #{:jmp :cjmp :ret :trap} (first instr)))

(defn instr-defs
  "Переменные, которые инструкция записывает."
  [instr]
  (case (first instr)
    (:mov :neg :aget :aset :len) [(second instr)]
    :bin [(nth instr 2)]
    :call (vec (second instr))
    []))

(def ^:private use-idx
  "Позиции операндов-чтений в инструкции (у :call на позиции 3 вектор аргументов)."
  {:mov [2] :neg [2] :bin [3 4] :aget [2 3] :aset [2 3 4] :len [2] :cjmp [2 3]})

(defn map-uses
  "Применяет f к каждому операнду-чтению инструкции (dst не трогает)."
  [instr f]
  (if (= :call (first instr))
    (update instr 3 #(mapv f %))
    (reduce #(update %1 %2 f) instr (use-idx (first instr)))))

(defn instr-uses
  "Переменные, которые инструкция читает (без `:ret`, ему нужны :returns функции)."
  [instr]
  (filterv variable?
           (if (= :call (first instr))
             (nth instr 3)
             (map #(nth instr %) (use-idx (first instr))))))

(defn cmp-fn [c]
  (case c :lt < :le <= :gt > :ge >= :eq = :ne not=))

(defn fold-bin
  "Значение целочисленной операции или nil (деление на 0)."
  [op x y]
  (case op
    :add (wrap32 (+ x y))
    :sub (wrap32 (- x y))
    :mul (wrap32 (* x y))
    :div (when-not (zero? y) (wrap32 (quot x y)))))

(defn may-trap?
  "Инструкция может бросить исключение (деление, индексация)."
  [instr]
  (or (and (= :bin (first instr)) (= :div (second instr)))
      (contains? #{:aget :aset} (first instr))))

(defn pure?
  "Нет эффектов, кроме записи в dst, и исключений быть не может."
  [instr]
  (and (contains? #{:mov :neg :bin :aget :aset :len} (first instr))
       (not (may-trap? instr))))


(defn instr->text [instr]
  (let [[op & args] instr]
    (case op
      :label (str (first args) ":")
      :mov (str "  " (first args) " = " (second args))
      :neg (str "  " (first args) " = -" (second args))
      :bin (let [[o dst a b] args]
             (str "  " dst " = " a " " ({:add "+" :sub "-" :mul "*" :div "/"} o) " " b))
      :aget (str "  " (first args) " = " (second args) "[" (nth args 2) "]")
      :aset (str "  " (first args) " = set(" (second args) ", " (nth args 2) ", " (nth args 3) ")")
      :len (str "  " (first args) " = length(" (second args) ")")
      :call (let [[dsts name as] args]
              (str "  " (str/join ", " dsts) " = call " name "(" (str/join ", " as) ")"))
      :jmp (str "  goto " (first args))
      :cjmp (let [[c a b lt lf] args]
              (str "  if " a " " ({:lt "<" :le "<=" :gt ">" :ge ">=" :eq "==" :ne "!="} c) " " b
                   " goto " lt " else " lf))
      :ret "  return"
      :trap (str "  trap " (name (first args))))))

(defn function->text [{:keys [name params returns code]}]
  (str "function " name "(" (str/join ", " params) ") returns " (str/join ", " returns) "\n"
       (str/join "\n" (map instr->text code))))

(defn module->text [functions]
  (str/join "\n\n" (map function->text functions)))


(def ^:private arith {:PLUS :add :MINUS :sub :STAR :mul :SLASH :div})
(def ^:private cmps {:EQEQ :eq :NE :ne :LE :le :GE :ge :LT :lt :GT :gt})
(def ^:private negate-cmp {:eq :ne :ne :eq :lt :ge :ge :lt :gt :le :le :gt})

(defn- new-gen [] {:code (atom []) :temps (atom 0) :labels (atom 0) :types (atom {})})

(defn- emit! [g instr] (swap! (:code g) conj instr))

(defn- new-temp! [g type]
  (let [t (str "%t" (swap! (:temps g) inc))]
    (swap! (:types g) assoc t type)
    t))

(defn- new-label! [g] (str "L" (swap! (:labels g) inc)))

(declare gen-expr! gen-cond!)

(defn- literal-value [node]
  (cond (= :int (:node node)) (long (:value node))
        (and (= :unary (:node node)) (= :MINUS (:operator node))) (when-let [v (literal-value (:operand node))] (- v))))

(defn gen-expr!
  "Генерирует код выражения, возвращает операнд. Если задан `dst`, результат
  вычисляется прямо в него."
  ([g expr] (gen-expr! g expr nil))
  ([g expr dst]
   (let [target (fn [type] (or dst (new-temp! g type)))
         atom-result (fn [operand] (if dst (do (emit! g [:mov dst operand]) dst) operand))]
     (if-let [c (literal-value expr)]
       (atom-result (wrap32 c))
       (case (:node expr)
         :name (atom-result (:name expr))
         :unary (let [a (gen-expr! g (:operand expr))
                      d (target :int)]
                  (emit! g [:neg d a])
                  d)
         :binary (let [a (gen-expr! g (:left expr))
                       b (gen-expr! g (:right expr))
                       d (target :int)]
                   (emit! g [:bin (arith (:operator expr)) d a b])
                   d)
         :index (let [arr (gen-expr! g (:target expr))
                      i (gen-expr! g (:index expr))
                      d (target :int)]
                  (emit! g [:aget d arr i])
                  d)
         :call (let [args (mapv #(gen-expr! g %) (:args expr))
                     d (target (:type expr))]
                 (if (= "length" (:name expr))
                   (emit! g [:len d (first args)])
                   (emit! g [:call [d] (:name expr) args]))
                 d))))))

(defn gen-cond!
  "Генерирует переходы по условию: на `lt`, если истинно, иначе на `lf`."
  [g expr lt lf]
  (case (:node expr)
    :bool (emit! g [:jmp (if (:value expr) lt lf)])
    :unary (gen-cond! g (:operand expr) lf lt)
    :binary
    (let [op (:operator expr)]
      (cond
        (contains? cmps op)
        (let [a (gen-expr! g (:left expr))
              b (gen-expr! g (:right expr))]
          (emit! g [:cjmp (cmps op) a b lt lf]))

        (#{:AND :OR :IMPLIES} op)
        (let [mid (new-label! g)
              [on-true on-false] (case op :AND [mid lf] :OR [lt mid] :IMPLIES [mid lt])]
          (gen-cond! g (:left expr) on-true on-false)
          (emit! g [:label mid])
          (gen-cond! g (:right expr) lt lf))))))

(defn- gen-stmt! [g opts stmt]
  (case (:node stmt)
    :block (doseq [s (:statements stmt)] (gen-stmt! g opts s))
    :assign (gen-expr! g (:value stmt) (:target stmt))
    :array-update (let [i (gen-expr! g (first (:indices stmt)))
                        v (gen-expr! g (:value stmt))]
                    (emit! g [:aset (:target stmt) (:target stmt) i v]))
    :tuple-assign (let [call (:value stmt)
                        args (mapv #(gen-expr! g %) (:args call))]
                    (emit! g [:call (vec (:targets stmt)) (:name call) args]))
    :if (let [lthen (new-label! g) lelse (new-label! g) lend (new-label! g)]
          (gen-cond! g (:condition stmt) lthen lelse)
          (emit! g [:label lthen])
          (gen-stmt! g opts (:then stmt))
          (emit! g [:jmp lend])
          (emit! g [:label lelse])
          (when (:else stmt) (gen-stmt! g opts (:else stmt)))
          (emit! g [:label lend]))
    :while (let [lcond (new-label! g) lbody (new-label! g) lend (new-label! g)]
             (emit! g [:label lcond])
             (gen-cond! g (:condition stmt) lbody lend)
             (emit! g [:label lbody])
             (gen-stmt! g opts (:body stmt))
             (emit! g [:jmp lcond])
             (emit! g [:label lend]))
    (:assert :assume)
    (when (:asserts opts)
      (let [lok (new-label! g) lfail (new-label! g)]
        (gen-cond! g (:predicate stmt) lok lfail)
        (emit! g [:label lfail])
        (emit! g [:trap (:node stmt)])
        (emit! g [:label lok])))))

(defn gen-function
  "IR функции из аннотированного узла :function (результат hw3)."
  [opts f]
  (let [g (new-gen)
        declared (concat (:params f) (:returns f) (:locals f))]
    (swap! (:types g) into (map (fn [v] [(:name v) (:type v)]) declared))
    (gen-stmt! g opts (:body f))
    (emit! g [:ret])
    {:name (:name f)
     :params (mapv :name (:params f))
     :returns (mapv :name (:returns f))
     :var-types @(:types g)
     :code @(:code g)}))

(defn gen-module
  "IR всех функций модуля (аннотированный AST из hw3)."
  ([ast] (gen-module ast {}))
  ([ast opts] (mapv #(gen-function opts %) (:functions ast))))

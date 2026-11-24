(ns t1.codegen
  "Кодогенерация T1: IR (TAC из hw4.ir, возможно оптимизированный hw4.opt) -> JVM-байт-код через ASM
  (ClassWriter + COMPUTE_FRAMES). Конвейер: AST -> hw4.ir/gen-module -> hw4.opt/optimize-module -> generate
  (см. t1.compile).

  ABI (см. спецификацию проекта):
  - один модуль -> один `public final` класс; одна функция -> один `public static` метод с тем же именем;
  - int -> JVM int (I); int[] -> t1/ImmutableIntArray (настоящий JVM-класс из java/t1);
  - несколько `returns`, порядок элементов = порядок `returns`, длина фиксирована:
    * только int -> JVM int[] ([I), как требует спецификация; вызывающая сторона читает через IALOAD;
    * есть хотя бы один int[] -> JVM Object[] ([Ljava/lang/Object;): int[] кортеж не может хранить
      ImmutableIntArray. Выбран именно Object[] (а не record/generic): это остаётся «кортежем как массивом
      фиксированной длины», не требует генерации лишних классов (record — бонус-вариант ABI) и не меняет
      ABI чисто int-кортежей. Элементы: int упаковывается в java/lang/Integer, массив лежит как есть
      (ImmutableIntArray). Вызывающая сторона распаковывает: AALOAD + CHECKCAST Integer + intValue()
      либо AALOAD + CHECKCAST ImmutableIntArray;
  - IR-инструкция `:aset` (функциональное обновление a[i] = v) -> ImmutableIntArray.with(i, v).

  Стратегия проверок времени выполнения: явных проверок НЕТ, полагаемся на JVM:
  - деление на ноль -> java.lang.ArithmeticException (команда IDIV);
  - выход за границы -> java.lang.ArrayIndexOutOfBoundsException (ImmutableIntArray.get/with
    обращаются к int[] напрямую; `with` бросает его до создания нового объекта).

  Прочее:
  - `assert`/`assume`/`requires`/`ensures`/`invariant` в T1 не компилируются (IR строится без :asserts);
  - все переменные IR (возвраты, локальные, временные %tN) в начале метода получают 0 (int) или пустой
    массив (int[]): верификатор JVM требует определённого присваивания, а Funny оставляет чтение
    неинициализированного неопределённым (запись в неинициализированный массив запрещает t1.semantic);
  - оператора `return` в Funny нет: IR-инструкция `:ret` возвращает текущие значения `returns`."
  (:import (org.objectweb.asm ClassWriter Label MethodVisitor Opcodes)))

(def ^:private array-class "t1/ImmutableIntArray")
(def ^:private array-desc (str "L" array-class ";"))

(defn- desc [t] (case t :int "I" :int-array array-desc))

(defn- fail [message data]
  (throw (ex-info message (assoc data :phase :codegen))))


(defn- signature
  "Сигнатура IR-функции: {:params [тип ...] :returns [тип ...]}."
  [{:keys [params returns var-types]}]
  {:params (mapv var-types params) :returns (mapv var-types returns)})

(defn- tuple-desc
  "JVM-тип кортежа из нескольких возвратов: [I для чистых int, иначе [Ljava/lang/Object;."
  [returns]
  (if (every? #{:int} returns) "[I" "[Ljava/lang/Object;"))

(defn- method-desc [{:keys [params returns]}]
  (str "(" (apply str (map desc params)) ")"
       (if (= 1 (count returns)) (desc (first returns)) (tuple-desc returns))))


(defn- push-int [^MethodVisitor mv n]
  (let [n (.intValue (biginteger n))]
    (cond (<= -1 n 5) (.visitInsn mv (+ Opcodes/ICONST_0 n))
          (<= -128 n 127) (.visitIntInsn mv Opcodes/BIPUSH n)
          (<= -32768 n 32767) (.visitIntInsn mv Opcodes/SIPUSH n)
          :else (.visitLdcInsn mv (Integer/valueOf n)))))

(defn- empty-array [^MethodVisitor mv]
  (.visitInsn mv Opcodes/ICONST_0)
  (.visitIntInsn mv Opcodes/NEWARRAY Opcodes/T_INT)
  (.visitMethodInsn mv Opcodes/INVOKESTATIC array-class "of" (str "([I)" array-desc) false))

(defn- slot-of [{:keys [slots]} name]
  (or (get slots name) (fail (str "Unknown variable '" name "'") {:name name})))

(defn- var-type [{:keys [var-types]} name] (get var-types name))

(defn- load-var [{:keys [^MethodVisitor mv] :as ctx} name]
  (.visitVarInsn mv (if (= :int (var-type ctx name)) Opcodes/ILOAD Opcodes/ALOAD) (slot-of ctx name)))

(defn- store-var [{:keys [^MethodVisitor mv] :as ctx} name]
  (.visitVarInsn mv (if (= :int (var-type ctx name)) Opcodes/ISTORE Opcodes/ASTORE) (slot-of ctx name)))

(defn- push-operand
  "Операнд IR: целое число — константа, строка — переменная."
  [{:keys [^MethodVisitor mv] :as ctx} operand]
  (if (integer? operand) (push-int mv operand) (load-var ctx operand)))


(def ^:private bin-op {:add Opcodes/IADD :sub Opcodes/ISUB :mul Opcodes/IMUL :div Opcodes/IDIV})

(def ^:private cmp-jump
  {:eq Opcodes/IF_ICMPEQ :ne Opcodes/IF_ICMPNE :lt Opcodes/IF_ICMPLT
   :ge Opcodes/IF_ICMPGE :gt Opcodes/IF_ICMPGT :le Opcodes/IF_ICMPLE})

(defn- label-for [{:keys [labels]} name]
  (let [ls (swap! labels (fn [m] (if (contains? m name) m (assoc m name (Label.)))))]
    (get ls name)))

(defn- gen-return [{:keys [^MethodVisitor mv returns] :as ctx}]
  (if (= 1 (count returns))
    (do (load-var ctx (first returns))
        (.visitInsn mv (if (= :int (var-type ctx (first returns))) Opcodes/IRETURN Opcodes/ARETURN)))
    (let [types (map #(var-type ctx %) returns)
          ints? (every? #{:int} types)]
      (push-int mv (count returns))
      (if ints?
        (.visitIntInsn mv Opcodes/NEWARRAY Opcodes/T_INT)
        (.visitTypeInsn mv Opcodes/ANEWARRAY "java/lang/Object"))
      (doseq [[i r t] (map vector (range) returns types)]
        (.visitInsn mv Opcodes/DUP)
        (push-int mv i)
        (load-var ctx r)
        (cond ints? (.visitInsn mv Opcodes/IASTORE)
              (= :int t) (do (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Integer" "valueOf"
                                               "(I)Ljava/lang/Integer;" false)
                             (.visitInsn mv Opcodes/AASTORE))
              :else (.visitInsn mv Opcodes/AASTORE)))
      (.visitInsn mv Opcodes/ARETURN))))

(defn- gen-call [{:keys [^MethodVisitor mv sigs owner tmp-slot] :as ctx} dsts name args]
  (let [sig (or (get sigs name) (fail (str "Unknown function '" name "'") {:name name}))]
    (doseq [a args] (push-operand ctx a))
    (.visitMethodInsn mv Opcodes/INVOKESTATIC owner name (method-desc sig) false)
    (case (count dsts)
      0 (.visitInsn mv Opcodes/POP)
      1 (store-var ctx (first dsts))
      (let [ints? (every? #{:int} (:returns sig))]
        (.visitVarInsn mv Opcodes/ASTORE tmp-slot)
        (doseq [[i d] (map-indexed vector dsts)]
          (.visitVarInsn mv Opcodes/ALOAD tmp-slot)
          (push-int mv i)
          (cond ints? (.visitInsn mv Opcodes/IALOAD)
                (= :int (var-type ctx d))
                (do (.visitInsn mv Opcodes/AALOAD)
                    (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Integer")
                    (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL "java/lang/Integer" "intValue" "()I" false))
                :else (do (.visitInsn mv Opcodes/AALOAD)
                          (.visitTypeInsn mv Opcodes/CHECKCAST array-class)))
          (store-var ctx d))))))

(defn- gen-instr [{:keys [^MethodVisitor mv] :as ctx} [op & args]]
  (case op
    :label (.visitLabel mv (label-for ctx (first args)))
    :mov (let [[dst a] args] (push-operand ctx a) (store-var ctx dst))
    :neg (let [[dst a] args] (push-operand ctx a) (.visitInsn mv Opcodes/INEG) (store-var ctx dst))
    :bin (let [[o dst a b] args]
           (push-operand ctx a) (push-operand ctx b)
           (.visitInsn mv (bin-op o))
           (store-var ctx dst))
    :aget (let [[dst arr i] args]
            (push-operand ctx arr) (push-operand ctx i)
            (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL array-class "get" "(I)I" false)
            (store-var ctx dst))
    :aset (let [[dst arr i v] args]
            (push-operand ctx arr) (push-operand ctx i) (push-operand ctx v)
            (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL array-class "with" (str "(II)" array-desc) false)
            (store-var ctx dst))
    :len (let [[dst arr] args]
           (push-operand ctx arr)
           (.visitMethodInsn mv Opcodes/INVOKEVIRTUAL array-class "length" "()I" false)
           (store-var ctx dst))
    :call (let [[dsts name as] args] (gen-call ctx dsts name as))
    :jmp (.visitJumpInsn mv Opcodes/GOTO (label-for ctx (first args)))
    :cjmp (let [[c a b lt lf] args]
            (push-operand ctx a) (push-operand ctx b)
            (.visitJumpInsn mv (cmp-jump c) (label-for ctx lt))
            (.visitJumpInsn mv Opcodes/GOTO (label-for ctx lf)))
    :ret (gen-return ctx)
    :trap (fail "assert/assume are not compiled in T1" {:instruction op})))


(defn- gen-function [^ClassWriter cw owner sigs {:keys [name params returns var-types code] :as f}]
  (let [mv (.visitMethod cw (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) name
                         (method-desc (signature f)) nil nil)
        others (sort (remove (set params) (keys var-types)))
        slots (into {} (map-indexed (fn [i n] [n i])) (concat params others))
        ctx {:mv mv :slots slots :var-types var-types :sigs sigs :owner owner
             :returns returns :labels (atom {}) :tmp-slot (count slots)}]
    (.visitCode mv)
    (doseq [n others]
      (if (= :int (var-types n)) (.visitInsn mv Opcodes/ICONST_0) (empty-array mv))
      (store-var ctx n))
    (run! #(gen-instr ctx %) code)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn generate
  "Строит байт-код модуля. `functions` — вектор IR-функций (hw4.ir/gen-module, при желании после
  hw4.opt/optimize-module); `class-name` — внутреннее имя класса (например \"funny/Module\").
  Возвращает byte[]."
  ^bytes [functions class-name]
  (let [cw (ClassWriter. ClassWriter/COMPUTE_FRAMES)
        sigs (into {} (map (juxt :name signature)) functions)]
    (.visit cw Opcodes/V21 (bit-or Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL Opcodes/ACC_SUPER)
            class-name nil "java/lang/Object" nil)
    (doseq [f functions] (gen-function cw class-name sigs f))
    (.visitEnd cw)
    (.toByteArray cw)))

(ns t1.semantic
  "Дополнительная семантическая проверка T1 (поверх hw3): запись в элемент массива `a[i] = v`
  разрешена, только если массив `a` уже целиком присвоен в этой функции раньше.
  Иначе возвращается ошибка {:phase :semantic :kind :uninitialized-array :line :column :message}.

  Работает по типизированному AST (до обессахаривания: узлы :array-update).

  Проверка — определённое присваивание по структуре программы, без анализа значений:
  - `a = <выражение>` и `a, b = f(...)` помечают массивы-цели присвоенными; `a = b` помечает `a`,
    только если `b` сам присвоен (или это параметр);
  - после `if` присвоенными остаются массивы, присвоенные в обеих ветках (без `else` — ничего нового);
  - присваивания внутри тела `while` после цикла не учитываются (тело может не выполниться);
  - массив, пришедший из вызова функции, считается присвоенным (содержимое вызываемой функции не анализируется)."
  (:require [clojure.set :as set]))

(defn- array-assign
  "Добавляет в `init` массивы-цели присваивания `stmt` (узел :assign или :tuple-assign)."
  [init params stmt]
  (case (:node stmt)
    :assign (let [{:keys [target value target-type]} stmt]
              (if (and (= :int-array target-type)
                       (or (not= :name (:node value))
                           (contains? init (:name value))
                           (contains? params (:name value))))
                (conj init target)
                init))
    :tuple-assign (into init
                        (keep (fn [[t type]] (when (= :int-array type) t)))
                        (map vector (:targets stmt) (:return-types (:value stmt))))))

(defn- walk
  "Обходит оператор в порядке исполнения, возвращает множество присвоенных массивов после него.
  Ошибки складывает в атом `errors`."
  [errors params init stmt]
  (case (:node stmt)
    :block (reduce #(walk errors params %1 %2) init (:statements stmt))
    (:assign :tuple-assign) (array-assign init params stmt)
    :array-update
    (do (when-not (or (contains? init (:target stmt)) (contains? params (:target stmt)))
          (swap! errors conj
                 {:phase :semantic :kind :uninitialized-array
                  :message (str "Array '" (:target stmt) "' is written before it is assigned as a whole")
                  :line (get-in stmt [:position :line]) :column (get-in stmt [:position :column])}))
        init)
    :if (let [then-init (walk errors params init (:then stmt))
              else-init (if (:else stmt) (walk errors params init (:else stmt)) init)]
          (set/intersection then-init else-init))
    :while (do (walk errors params init (:body stmt)) init)
    init))

(defn check
  "Проверяет типизированный AST модуля (:ast из t0.pipeline/run). Возвращает вектор ошибок."
  [module]
  (let [errors (atom [])]
    (doseq [f (:functions module)]
      (walk errors (set (map :name (:params f))) #{} (:body f)))
    (vec (sort-by (juxt :line :column) @errors))))

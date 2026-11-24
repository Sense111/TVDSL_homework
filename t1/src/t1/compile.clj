(ns t1.compile
  "Вход компилятора T1: строка Funny -> t0.pipeline/run (типы hw3) -> t1.semantic -> IR (hw4.ir) ->
  оптимизации (hw4.opt) -> байт-код. Ошибки возвращаются данными."
  (:require [hw4.ir :as ir]
            [hw4.opt :as opt]
            [t0.pipeline :as pipeline]
            [t1.codegen :as codegen]
            [t1.loader :as loader]
            [t1.semantic :as semantic]))

(defn- to-ir
  "IR модуля из типизированного AST. Берётся :ast (с узлами :array-update): hw4.ir сам превращает
  a[i] = v в функциональное обновление :aset, поэтому обессахаренное дерево не нужно."
  [ast opts]
  (let [functions (ir/gen-module ast)]
    (if (false? (:optimize opts))
      functions
      (opt/optimize-module functions (dissoc opts :optimize)))))

(defn compile-source
  "Компилирует модуль Funny. `class-name` — имя с точками, по умолчанию \"funny.Module\".
  `opts` (необязательно): {:optimize false} отключает оптимизации целиком; иначе флаги hw4.opt
  (:const-prop :algebra :dce :cse; по умолчанию const-prop, algebra и dce включены, cse выключен).
  Успех: {:class-name \"funny.Module\" :bytes byte[] :ir [...]}.
  Ошибка (синтаксис/типы/семантика/кодогенерация): {:errors [{...}]}."
  ([source] (compile-source source "funny.Module" {}))
  ([source class-name] (compile-source source class-name {}))
  ([source class-name opts]
   (let [{:keys [ast errors]} (pipeline/run source)
         errors (if (and ast (empty? errors)) (semantic/check ast) errors)]
     (cond
       (seq errors) {:errors errors}
       (nil? ast) {:errors [{:phase :syntax :message "Не удалось получить AST"}]}
       :else
       (try
         (let [functions (to-ir ast opts)]
           {:class-name class-name
            :ir functions
            :bytes (codegen/generate functions (.replace ^String class-name \. \/))})
         (catch clojure.lang.ExceptionInfo e
           {:errors [(assoc (ex-data e) :phase :codegen :message (ex-message e))]}))))))

(defn load-module
  "Компилирует и загружает; возвращает {:class Class} или {:errors [...]}. Аргументы как у compile-source."
  ([source] (load-module source "funny.Module" {}))
  ([source class-name] (load-module source class-name {}))
  ([source class-name opts]
   (let [{:keys [bytes errors] :as result} (compile-source source class-name opts)]
     (if errors
       result
       (assoc result :class (loader/define-class class-name bytes))))))

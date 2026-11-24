(ns t0.pipeline
  "Фронтенд T0: лексер HW1 -> парсер HW2 -> типы HW3 плюс обессахаривание обновления массива."
  (:require [hw1.lexer :as lexer]
            [hw3.types :as types]))

(defonce ^:private compiled-lexer (delay (lexer/compile-lexer)))

(defn tokens
  "Токены `source` с позициями (HW1) или nil при лексической ошибке."
  [source]
  (try (lexer/scan @compiled-lexer source)
       (catch clojure.lang.ExceptionInfo _ nil)))

(defn- set-call [target index value position]
  {:node :call :position position :name "set" :type :int-array
   :args [target index value]})

(defn- desugar-update
  "a[i1]...[in] = v  ==>  a = set(a, i1, set(a[i1], i2, ... v))   (функциональное обновление)."
  [{:keys [target indices value position] :as stmt}]
  (let [base {:node :name :position position :name target :type :int-array}
        build (fn build [array indices]
                (let [[i & more] indices]
                  (set-call array i
                            (if (seq more)
                              (build {:node :index :position position :target array :index i :type :int}
                                     more)
                              value)
                            position)))]
    (-> {:node :assign :position position :target target
         :value (build base indices) :target-type :int-array :desugared-from :array-update}
        (cond-> (:inferred? stmt) (assoc :inferred? true)))))

(defn desugar
  "Заменяет каждый узел :array-update явным присваиванием с функциональным обновлением."
  [tree]
  (cond
    (map? tree) (let [tree (into {} (map (fn [[k v]] [k (desugar v)])) tree)]
                  (if (= :array-update (:node tree)) (desugar-update tree) tree))
    (vector? tree) (mapv desugar tree)
    :else tree))

(defn run
  "Полный конвейер T0: {:tokens :ast :desugared :errors}. При любых ошибках :desugared равен nil."
  [source]
  (let [{:keys [ast errors]} (types/check-source source)]
    {:tokens (tokens source)
     :ast ast
     :desugared (when (and ast (empty? errors)) (desugar ast))
     :errors errors}))

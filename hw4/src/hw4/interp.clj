(ns hw4.interp
  "Интерпретатор IR (IR.md). Нужен для проверки, что оптимизации не меняют
  результат. Массивы - векторы целых, целые - 32-битные со знаком."
  (:require [hw4.ir :as ir]))

(defn- trap! [type message data]
  (throw (ex-info message (assoc data :type type))))

(defn- checked-index [a idx]
  (if (< -1 idx (count a))
    idx
    (trap! :index "index out of bounds" {:index idx :length (count a)})))

(defn- label-index [code]
  (into {} (keep-indexed (fn [i instr] (when (= :label (first instr)) [(second instr) i]))) code))

(declare call-function)

(defn- run-code [module f args depth fuel]
  (let [code (:code f)
        labels (label-index code)
        n (count code)
        default (fn [name] (if (= :int-array (get-in f [:var-types name])) [] 0))
        value (fn [env o] (if (integer? o) o (get env o (default o))))
        results (fn [env] (mapv #(get env % (default %)) (:returns f)))
        env0 (zipmap (:params f) args)]
    (loop [pc 0, env env0]
      (when (neg? (swap! fuel dec))
        (trap! :fuel "step limit exceeded" {}))
      (if (>= pc n)
        (results env)
        (let [instr (nth code pc)
              v #(value env %)]
          (case (first instr)
            :label (recur (inc pc) env)
            :mov (recur (inc pc) (assoc env (nth instr 1) (v (nth instr 2))))
            :neg (recur (inc pc) (assoc env (nth instr 1) (ir/wrap32 (- (v (nth instr 2))))))
            :bin (let [[_ op dst a b] instr
                       r (ir/fold-bin op (v a) (v b))]
                   (when (nil? r) (trap! :div-zero "division by zero" {}))
                   (recur (inc pc) (assoc env dst r)))
            :aget (let [[_ dst arr i] instr
                        a (v arr)]
                    (recur (inc pc) (assoc env dst (nth a (checked-index a (v i))))))
            :aset (let [[_ dst arr i x] instr
                        a (v arr)]
                    (recur (inc pc) (assoc env dst (assoc a (checked-index a (v i)) (v x)))))
            :len (recur (inc pc) (assoc env (nth instr 1) (count (v (nth instr 2)))))
            :call (let [[_ dsts name as] instr
                        results (call-function module name (mapv v as) (inc depth) fuel)]
                    (recur (inc pc) (into env (zipmap dsts results))))
            :jmp (recur (get labels (second instr)) env)
            :cjmp (let [[_ c a b lt lf] instr]
                    (recur (get labels (if ((ir/cmp-fn c) (v a) (v b)) lt lf)) env))
            :ret (results env)
            :trap (trap! (second instr) (str "trap " (name (second instr))) {})))))))

(defn- call-function [module name args depth fuel]
  (let [f (some #(when (= name (:name %)) %) module)]
    (when-not f (trap! :no-function (str "unknown function " name) {}))
    (when (> depth 200) (trap! :depth "call depth exceeded" {}))
    (run-code module f args depth fuel)))

(defn run
  "Запускает функцию `name` модуля IR с аргументами (целые и векторы).
  Возвращает вектор возвращаемых значений. Исключения исполнения - ex-info
  с :type из #{:div-zero :index :assert :assume :fuel :depth :no-function}."
  ([module name args] (run module name args 1000000))
  ([module name args fuel]
   (call-function module name (vec args) 0 (atom fuel))))

(defn outcome
  "Как `run`, но исключения исполнения превращает в данные: {:ok [..]} или {:error type}."
  [module name args]
  (try {:ok (run module name args)}
       (catch clojure.lang.ExceptionInfo e {:error (:type (ex-data e))})))

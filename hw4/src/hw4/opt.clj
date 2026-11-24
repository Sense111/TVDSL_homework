(ns hw4.opt
  "Оптимизации IR: распространение констант и копий, алгебраические
  упрощения, удаление мёртвого кода (по глобальной живости), CSE в блоке."
  (:require [hw4.cfg :as cfg]
            [hw4.ir :as ir]
            [hw4.regalloc :as regalloc]))

(def default-opts {:const-prop true :algebra true :dce true :cse false})

(defn- enabled? [opts k] (get (merge default-opts opts) k))


(defn- subst-uses
  "Подставляет в операнды-чтения значения из карты env (dst не трогает)."
  [instr env]
  (ir/map-uses instr (fn [o] (if (ir/variable? o) (get env o o) o))))

(defn- fold-instr [instr]
  (case (first instr)
    :neg (let [a (nth instr 2)]
           (if (ir/const? a) [:mov (nth instr 1) (ir/wrap32 (- a))] instr))
    :bin (let [[_ op dst a b] instr]
           (if (and (ir/const? a) (ir/const? b))
             (if-some [v (ir/fold-bin op a b)] [:mov dst v] instr)
             instr))
    :cjmp (let [[_ c a b lt lf] instr]
            (if (and (ir/const? a) (ir/const? b))
              [:jmp (if ((ir/cmp-fn c) a b) lt lf)]
              instr))
    instr))


(defn const-prop-block
  "Локальное распространение констант и копий + свёртка. Факты: var -> константа
  или другая переменная; сбрасываются при переопределении любой из сторон."
  [instrs]
  (loop [is instrs, env {}, out []]
    (if-let [instr (first is)]
      (let [instr (fold-instr (subst-uses instr env))
            defs (set (ir/instr-defs instr))
            env (into {} (remove (fn [[k v]] (or (defs k) (defs v)))) env)
            env (if (and (= :mov (first instr)) (not= (nth instr 1) (nth instr 2)))
                  (assoc env (nth instr 1) (nth instr 2))
                  env)]
        (recur (rest is) env (conj out instr)))
      out)))


(defn- simplify-instr [instr]
  (case (first instr)
    :bin (let [[_ op dst a b] instr]
           (case op
             :add (cond (= b 0) [:mov dst a]
                        (= a 0) [:mov dst b]
                        :else instr)
             :sub (cond (= b 0) [:mov dst a]
                        (and (string? a) (= a b)) [:mov dst 0]
                        :else instr)
             :mul (cond (or (= a 0) (= b 0)) [:mov dst 0]
                        (= b 1) [:mov dst a]
                        (= a 1) [:mov dst b]
                        :else instr)
             :div (if (= b 1) [:mov dst a] instr)))
    :cjmp (let [[_ c a b lt lf] instr]
            (if (and (string? a) (= a b))
              [:jmp (if (#{:eq :le :ge} c) lt lf)]
              instr))
    instr))

(defn algebra-block [instrs] (mapv simplify-instr instrs))


(defn dce-block
  "Удаляет чистые инструкции, все dst которых не живы после них. `out` -
  живые на выходе блока."
  [instrs out ret-uses]
  (let [la (cfg/live-after instrs out ret-uses)]
    (into []
          (keep-indexed
           (fn [i instr]
             (let [dead? (and (ir/pure? instr)
                              (or (not-any? (nth la i) (ir/instr-defs instr))
                                  (and (= :mov (first instr)) (= (nth instr 1) (nth instr 2)))))]
               (when-not dead? instr))))
          instrs)))


(defn- cse-key [instr]
  (case (first instr)
    :bin (let [[_ op _ a b] instr]
           (if (#{:add :mul} op)
             [:bin op (vec (sort-by str [a b]))]
             [:bin op [a b]]))
    :neg [:neg (nth instr 2)]
    :len [:len (nth instr 2)]
    :aget [:aget (nth instr 2) (nth instr 3)]
    nil))

(defn cse-block
  "Value numbering в блоке: повторное вычисление заменяется на [:mov dst earlier].
  Заменяемая инструкция уже выполнялась с теми же операндами, поэтому
  исключений не добавляет и не убирает."
  [instrs]
  (loop [is instrs, table {}, out []]
    (if-let [instr (first is)]
      (let [k (cse-key instr)
            holder (when k (get-in table [k :holder]))
            instr' (if holder [:mov (first (ir/instr-defs instr)) holder] instr)
            defs (set (ir/instr-defs instr'))
            table (into {} (remove (fn [[_ {:keys [holder operands]}]]
                                     (or (defs holder) (some defs operands))))
                        table)
            table (if (and k (not holder) (not-any? (set (ir/instr-uses instr)) defs))
                    (assoc table k {:holder (first (ir/instr-defs instr))
                                    :operands (ir/instr-uses instr)})
                    table)]
        (recur (rest is) table (conj out instr')))
      out)))


(defn- map-blocks [g f]
  (update g :blocks
          (fn [bs] (into {} (map (fn [[id b]] [id (update b :instrs f)])) bs))))

(defn- clean-cfg [g]
  (-> g cfg/refresh-succs cfg/remove-unreachable))

(defn dce
  "DCE по всему CFG до неподвижной точки."
  [g returns]
  (loop [g g]
    (let [live (cfg/liveness g returns)
          g' (reduce (fn [g id]
                       (update-in g [:blocks id :instrs]
                                  #(dce-block % (get-in live [id :out]) returns)))
                     g (:order g))]
      (if (= g' g) g (recur g')))))

(defn optimize-function
  [f opts]
  (let [returns (:returns f)
        step (fn [g]
               (cond-> g
                 (enabled? opts :const-prop) (-> (map-blocks const-prop-block) clean-cfg)
                 (enabled? opts :algebra) (-> (map-blocks algebra-block) clean-cfg)
                 (enabled? opts :cse) (map-blocks cse-block)
                 (enabled? opts :dce) (dce returns)))
        g (clean-cfg (cfg/build (:code f)))
        g (loop [g g, n 0]
            (let [g' (step g)]
              (if (or (= g' g) (>= n 20)) g' (recur g' (inc n)))))
        optimized (assoc f :code (cfg/linearize g))
        needed (into (set (regalloc/temps optimized)) (concat (:params f) returns))
        unused-temps (remove needed (filter regalloc/temp? (keys (:var-types f))))]
    (update optimized :var-types #(apply dissoc % unused-temps))))

(defn optimize-module [functions opts]
  (mapv #(optimize-function % opts) functions))

(defn metrics [f]
  {:instructions (count (remove #(= :label (first %)) (:code f)))
   :temps (count (regalloc/temps f))})

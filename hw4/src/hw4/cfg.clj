(ns hw4.cfg
  "CFG и базовые блоки поверх IR (формат см. IR.md), достижимость, живость, DOT."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [hw4.ir :as ir]))

(defn- successors [terminator]
  (case (first terminator)
    :jmp [(second terminator)]
    :cjmp (vec (distinct [(nth terminator 4) (nth terminator 5)]))
    []))

(defn- split-blocks
  "Делит линейный код на блоки: [[id instrs] ...], каждый блок без меток внутри.
  Блоки без терминатора `build` замыкает переходом на следующий."
  [code]
  (let [labels (into #{} (keep (fn [[op id]] (when (= op :label) id))) code)
        counter (atom -1)
        fresh (fn []
                (loop []
                  (let [id (str "B" (swap! counter inc))]
                    (if (contains? labels id) (recur) id))))]
    (loop [instrs code, current nil, blocks []]
      (if-let [instr (first instrs)]
        (if (= :label (first instr))
          (recur (rest instrs) {:id (second instr) :instrs []} (cond-> blocks current (conj current)))
          (let [current (update (or current {:id (fresh) :instrs []}) :instrs conj instr)]
            (if (ir/terminator? instr)
              (recur (rest instrs) nil (conj blocks current))
              (recur (rest instrs) current blocks))))
        (cond-> blocks current (conj current))))))

(defn refresh-succs
  "Пересчитывает :succs по терминаторам (после оптимизаций, меняющих переходы)."
  [cfg]
  (update cfg :blocks update-vals #(assoc % :succs (successors (peek (:instrs %))))))

(defn build
  "Строит CFG из линейного кода функции. Блок без терминатора получает :jmp на
  следующий (или :ret, если он последний). Недостижимые блоки не удаляются."
  [code]
  (let [blocks (split-blocks code)
        blocks (vec (map-indexed
                     (fn [i b]
                       (if (and (seq (:instrs b)) (ir/terminator? (peek (:instrs b))))
                         b
                         (update b :instrs conj
                                 (if-let [nxt (get blocks (inc i))] [:jmp (:id nxt)] [:ret]))))
                     blocks))]
    (refresh-succs {:entry (:id (first blocks))
                    :order (mapv :id blocks)
                    :blocks (into {} (map (juxt :id identity)) blocks)})))

(defn preds
  "Карта: id блока -> вектор id предшественников."
  [cfg]
  (reduce (fn [m id]
            (reduce (fn [m s] (update m s (fnil conj []) id)) m (get-in cfg [:blocks id :succs])))
          (zipmap (:order cfg) (repeat []))
          (:order cfg)))

(defn reachable
  "Множество id блоков, достижимых из входа."
  [cfg]
  (loop [seen #{(:entry cfg)}, stack [(:entry cfg)]]
    (if-let [id (peek stack)]
      (let [new (remove seen (get-in cfg [:blocks id :succs]))]
        (recur (into seen new) (into (pop stack) new)))
      seen)))

(defn remove-unreachable [cfg]
  (let [live (reachable cfg)]
    (-> cfg
        (update :order #(filterv live %))
        (update :blocks #(select-keys % live)))))

(defn linearize
  "Обратно в линейный код: метка блока + его инструкции. Лишний :jmp на
  следующий блок убирается."
  [cfg]
  (let [order (:order cfg)
        incoming (preds cfg)]
    (vec (mapcat
          (fn [i id]
            (let [instrs (get-in cfg [:blocks id :instrs])
                  nxt (get order (inc i))
                  instrs (if (and nxt (= [:jmp nxt] (peek instrs))) (pop instrs) instrs)]
              (if (and (= id (:entry cfg)) (empty? (get incoming id)))
                instrs
                (cons [:label id] instrs))))
          (range) order))))


(defn- block-use-def [instrs ret-uses]
  (reduce (fn [{:keys [use def]} instr]
            (let [uses (into (ir/instr-uses instr) (when (= :ret (first instr)) ret-uses))]
              {:use (into use (remove def uses))
               :def (into def (ir/instr-defs instr))}))
          {:use #{} :def #{}}
          instrs))

(defn liveness
  "Итеративный анализ живости. `ret-uses` — переменные, живые на :ret (возвраты
  функции). Результат: {id {:in #{..} :out #{..}}}."
  [cfg ret-uses]
  (let [ids (:order cfg)
        ud (into {} (map (fn [id] [id (block-use-def (get-in cfg [:blocks id :instrs]) ret-uses)])) ids)]
    (loop [state (zipmap ids (repeat {:in #{} :out #{}}))]
      (let [next-state
            (reduce
             (fn [st id]
               (let [out (reduce set/union #{} (map #(get-in st [% :in] #{})
                                                    (get-in cfg [:blocks id :succs])))
                     in (set/union (get-in ud [id :use]) (set/difference out (get-in ud [id :def])))]
                 (assoc st id {:in in :out out})))
             state (reverse ids))]
        (if (= next-state state) state (recur next-state))))))

(defn live-after
  "Для каждой инструкции блока — множество переменных, живых сразу после неё."
  [instrs out ret-uses]
  (let [step (fn [live instr]
               (let [uses (into (ir/instr-uses instr) (when (= :ret (first instr)) ret-uses))]
                 (set/union (set/difference live (set (ir/instr-defs instr))) (set uses))))]
    (->> (reverse instrs)
         (reductions (fn [[live _] instr] [(step live instr) live]) [out out])
         rest
         (map second)
         reverse
         vec)))


(defn ->text [cfg]
  (str/join "\n"
            (for [id (:order cfg)
                  :let [b (get-in cfg [:blocks id])]]
              (str id ": -> [" (str/join ", " (:succs b)) "]\n"
                   (str/join "\n" (map ir/instr->text (:instrs b)))))))

(defn ->dot [name cfg]
  (str "digraph " (pr-str name) " {\n  node [shape=box fontname=monospace];\n"
       (str/join
        (for [id (:order cfg)
              :let [b (get-in cfg [:blocks id])
                    label (str id ":\\l" (str/join "" (map #(str (str/replace (ir/instr->text %) #"^ +" "") "\\l") (:instrs b))))]]
          (str "  " (pr-str id) " [label=\"" (str/replace label "\"" "\\\"") "\"];\n"
               (str/join (for [s (:succs b)] (str "  " (pr-str id) " -> " (pr-str s) ";\n"))))))
       "}\n"))

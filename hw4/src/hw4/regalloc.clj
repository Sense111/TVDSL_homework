(ns hw4.regalloc
  "Раскраска графа интерференции временных (%t...) — распределение регистров."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [hw4.cfg :as cfg]
            [hw4.ir :as ir]))

(defn temp? [x] (and (string? x) (str/starts-with? x "%")))

(defn temps
  "Отсортированный вектор временных, встречающихся в коде функции."
  [f]
  (->> (:code f)
       (mapcat #(concat (ir/instr-defs %) (ir/instr-uses %)))
       (filter temp?)
       distinct sort vec))

(defn- add-edge [g a b]
  (if (= a b) g (-> g (update a (fnil conj #{}) b) (update b (fnil conj #{}) a))))

(defn interference-graph
  "{:nodes [temps] :edges {temp #{neighbors}}}. Живость считается по всем
  переменным, узлами становятся только временные."
  [f]
  (let [g (cfg/build (:code f))
        ret-uses (vec (:returns f))
        lv (cfg/liveness g ret-uses)
        nodes (temps f)
        pairs (for [id (:order g)
                    :let [instrs (get-in g [:blocks id :instrs])
                          afters (cfg/live-after instrs (get-in lv [id :out]) ret-uses)]
                    [instr live] (map vector instrs afters)
                    :let [defs (filter temp? (ir/instr-defs instr))
                          src (when (= :mov (first instr)) (nth instr 2))]
                    pair (concat (for [d defs x live :when (and (temp? x) (not= x src))] [d x])
                                 (for [a defs b defs :when (neg? (compare a b))] [a b]))]
                pair)]
    {:nodes nodes
     :edges (reduce (fn [e [a b]] (add-edge e a b)) (zipmap nodes (repeat #{})) pairs)}))

(defn color-graph
  "Жадная раскраска: порядок по убыванию степени (ничьи — по имени),
  каждому наименьший свободный цвет (с 0)."
  [{:keys [nodes edges]}]
  (let [order (sort-by (fn [n] [(- (count (get edges n))) n]) nodes)]
    (reduce (fn [colors n]
              (let [used (set (keep colors (get edges n)))]
                (assoc colors n (first (remove used (range))))))
            {} order)))

(defn valid-coloring?
  [{:keys [nodes edges]} colors]
  (and (every? #(contains? colors %) nodes)
       (every? (fn [[a ns]] (every? #(not= (colors a) (colors %)) ns)) edges)))

(defn allocate
  ([f] (allocate f {}))
  ([f {:keys [k]}]
   (let [g (interference-graph f)
         colors (color-graph g)]
     (cond-> {:graph g
              :colors colors
              :num-colors (count (set (vals colors)))
              :temps (count (:nodes g))}
       k (assoc :k k
                :spilled (vec (filter #(>= (colors %) k) (:nodes g))))))))

(defn graph->text [{:keys [nodes edges]} colors]
  (str/join "\n"
            (for [n nodes]
              (str n " [color " (get colors n) "]:"
                   (apply str (map #(str " " %) (sort (get edges n))))))))

(defn graph->dot [name {:keys [nodes edges]} colors]
  (str "graph " (pr-str name) " {\n  node [shape=circle fontname=monospace];\n"
       (str/join (for [n nodes]
                   (str "  " (pr-str n) " [label=\"" n "\\n" (get colors n) "\"];\n")))
       (str/join (for [a nodes b (sort (get edges a)) :when (neg? (compare a b))]
                   (str "  " (pr-str a) " -- " (pr-str b) ";\n")))
       "}\n"))

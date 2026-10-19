(ns hw1.automata
  (:require [clojure.set :as set]))

(defn compile-nfa
  "Compile a regex AST to a Thompson epsilon-NFA."
  [ast]
  (let [next-state (atom 0)
        transitions (atom {})
        epsilon (atom {})
        new-state (fn [] (let [id @next-state] (swap! next-state inc) id))
        add-epsilon! (fn [from to] (swap! epsilon update from (fnil conj #{}) to))
        add-edge! (fn [from chars to] (swap! transitions update from (fnil conj []) [chars to]))]
    (letfn [(fragment [node]
              (let [[op arg] node]
                (case op
                  :epsilon
                  (let [start (new-state) accept (new-state)]
                    (add-epsilon! start accept)
                    {:start start :accept accept})

                  :char
                  (let [start (new-state) accept (new-state)]
                    (add-edge! start #{arg} accept)
                    {:start start :accept accept})

                  :class
                  (let [start (new-state) accept (new-state)]
                    (add-edge! start arg accept)
                    {:start start :accept accept})

                  :concat
                  (let [parts (mapv fragment arg)]
                    (doseq [[left right] (partition 2 1 parts)]
                      (add-epsilon! (:accept left) (:start right)))
                    {:start (:start (first parts)) :accept (:accept (last parts))})

                  :alt
                  (let [start (new-state) accept (new-state)
                        parts (mapv fragment arg)]
                    (doseq [part parts]
                      (add-epsilon! start (:start part))
                      (add-epsilon! (:accept part) accept))
                    {:start start :accept accept})

                  :star
                  (let [start (new-state) accept (new-state) part (fragment arg)]
                    (add-epsilon! start accept)
                    (add-epsilon! start (:start part))
                    (add-epsilon! (:accept part) (:start part))
                    (add-epsilon! (:accept part) accept)
                    {:start start :accept accept})

                  :plus
                  (let [start (new-state) accept (new-state) part (fragment arg)]
                    (add-epsilon! start (:start part))
                    (add-epsilon! (:accept part) (:start part))
                    (add-epsilon! (:accept part) accept)
                    {:start start :accept accept})

                  :optional
                  (let [start (new-state) accept (new-state) part (fragment arg)]
                    (add-epsilon! start accept)
                    (add-epsilon! start (:start part))
                    (add-epsilon! (:accept part) accept)
                    {:start start :accept accept})

                  (throw (ex-info "Unknown regex AST node" {:node node})))))]
      (let [{:keys [start accept]} (fragment ast)]
        {:start start
         :accept accept
         :transitions @transitions
         :epsilon @epsilon}))))

(defn- epsilon-closure [nfa states]
  (loop [closed (set states)
         pending (seq states)]
    (if-let [state (first pending)]
      (let [fresh (remove closed (get-in nfa [:epsilon state] #{}))]
        (recur (into closed fresh) (concat (next pending) fresh)))
      closed)))

(defn nfa-matches?
  "Return true when the NFA accepts the complete input string."
  [nfa input]
  (loop [active (epsilon-closure nfa #{(:start nfa)})
         remaining (seq input)]
    (if-let [ch (first remaining)]
      (let [destinations (for [state active
                               [chars target] (get-in nfa [:transitions state] [])
                               :when (contains? chars ch)]
                           target)]
        (recur (epsilon-closure nfa destinations) (next remaining)))
      (contains? active (:accept nfa)))))

(defn- nfa-states [nfa]
  (set (concat [(:start nfa) (:accept nfa)]
               (keys (:transitions nfa))
               (mapcat (fn [[_ edges]] (map second edges)) (:transitions nfa))
               (keys (:epsilon nfa))
               (mapcat val (:epsilon nfa))
               (keys (:accepts nfa)))))

(defn combine-nfas
  "Join token NFAs under one start state. Lower priority numbers win ties."
  [entries]
  (let [rebased
        (loop [remaining entries, offset 0, transitions {}, epsilon {}, accepts {}, starts []]
          (if-let [{:keys [token priority nfa]} (first remaining)]
            (let [remap (fn [state] (+ offset state))
                  states (nfa-states nfa)
                  next-offset (if (seq states) (+ offset 1 (apply max states)) offset)
                  transitions' (reduce-kv
                                (fn [result from edges]
                                  (assoc result (remap from)
                                         (mapv (fn [[chars to]] [chars (remap to)]) edges)))
                                transitions (:transitions nfa))
                  epsilon' (reduce-kv
                            (fn [result from destinations]
                              (assoc result (remap from) (set (map remap destinations))))
                            epsilon (:epsilon nfa))]
              (recur (next remaining) next-offset transitions' epsilon'
                     (assoc accepts (remap (:accept nfa)) {:token token :priority priority})
                     (conj starts (remap (:start nfa)))))
            {:offset offset :transitions transitions :epsilon epsilon
             :accepts accepts :starts starts}))
        root (:offset rebased)]
    (when (empty? entries)
      (throw (ex-info "At least one token NFA is required" {})))
    (assoc (dissoc rebased :offset :starts)
           :start root
           :epsilon (update (:epsilon rebased) root (fnil into #{}) (:starts rebased)))))

(defn- accepted-label [nfa subset]
  (->> subset
       (keep (fn [state]
               (when-let [accept (get-in nfa [:accepts state])]
                 [(:priority accept) (:token accept)])))
       (sort-by first)
       first
       second))

(defn determinize
  "Convert an epsilon-NFA into a total DFA over the supplied alphabet."
  [nfa alphabet]
  (let [alphabet (vec (sort-by int alphabet))
        initial (epsilon-closure nfa #{(:start nfa)})
        ids (atom {initial 0})
        queue (atom (conj clojure.lang.PersistentQueue/EMPTY initial))
        transitions (atom {})
        accepts (atom {})]
    (while (seq @queue)
      (let [subset (peek @queue)
            state (get @ids subset)]
        (swap! queue pop)
        (when-let [label (accepted-label nfa subset)]
          (swap! accepts assoc state label))
        (doseq [ch alphabet]
          (let [destinations (for [from subset
                                   [chars to] (get-in nfa [:transitions from] [])
                                   :when (contains? chars ch)]
                               to)
                target (epsilon-closure nfa destinations)]
            (when-not (contains? @ids target)
              (swap! ids assoc target (count @ids))
              (swap! queue conj target))
            (swap! transitions assoc-in [state ch] (get @ids target))))))
    {:start 0
     :alphabet (set alphabet)
     :trap (get @ids #{})
     :transitions @transitions
     :accepts @accepts}))

(defn- dfa-states [dfa]
  (set (remove nil? (concat [(:start dfa) (:trap dfa)]
                            (keys (:transitions dfa))
                            (mapcat vals (vals (:transitions dfa)))
                            (keys (:accepts dfa))))))

(defn- complete-dfa [dfa]
  (let [alphabet (or (:alphabet dfa)
                     (set (mapcat keys (vals (:transitions dfa)))))
        states (dfa-states dfa)
        trap (or (:trap dfa) (if (seq states) (inc (apply max states)) 0))
        states (conj states trap)
        transitions (reduce
                     (fn [table state]
                       (assoc table state
                              (into {}
                                    (map (fn [ch]
                                           [ch (get-in dfa [:transitions state ch] trap)])
                                         alphabet))))
                     (:transitions dfa) states)
        transitions (assoc transitions trap (zipmap alphabet (repeat trap)))]
    (assoc dfa :alphabet alphabet :trap trap :transitions transitions)))

(defn- reachable-states [dfa]
  (loop [seen #{(:start dfa)}, pending [(:start dfa)]]
    (if-let [state (first pending)]
      (let [fresh (remove seen (vals (get-in dfa [:transitions state])))]
        (recur (into seen fresh) (into (subvec pending 1) fresh)))
      seen)))

(defn- block-key [block]
  (apply min block))

(defn- split-blocks [blocks work preimage]
  (reduce
   (fn [[current worklist] block]
     (let [inside (set/intersection block preimage)
           outside (set/difference block preimage)]
       (if (or (empty? inside) (empty? outside))
         [current worklist]
         (let [current (-> current (disj block) (conj inside outside))
               worklist (if (contains? worklist block)
                          (-> worklist (disj block) (conj inside outside))
                          (conj worklist (if (< (count inside) (count outside)) inside outside)))]
           [current worklist]))))
   [blocks work]
   blocks))

(defn minimize
  "Minimize a total DFA using Hopcroft partition refinement."
  [dfa]
  (let [dfa (complete-dfa dfa)
        alphabet (vec (sort-by int (:alphabet dfa)))
        states (conj (reachable-states dfa) (:trap dfa))
        blocks (->> states
                    (group-by #(get-in dfa [:accepts %]))
                    vals
                    (map set)
                    set)
        initial-work blocks
        final-blocks
        (loop [blocks blocks, work initial-work]
          (if (empty? work)
            blocks
            (let [active (first (sort-by block-key work))
                  work (disj work active)
                  [blocks work]
                  (reduce
                   (fn [[current pending] ch]
                     (let [preimage (set (filter #(contains? active
                                                           (get-in dfa [:transitions % ch]))
                                                 states))]
                       (split-blocks current pending preimage)))
                   [blocks work]
                   alphabet)]
              (recur blocks work))))
        ordered-blocks (sort-by block-key final-blocks)
        block-ids (into {} (map-indexed (fn [id block] [block id]) ordered-blocks))
        state-to-block (into {} (mapcat (fn [[block id]] (map #(vector % id) block)) block-ids))
        minimized-transitions
        (into {}
              (map (fn [[block id]]
                     (let [representative (block-key block)]
                       [id (into {}
                                 (map (fn [ch]
                                        [ch (get state-to-block
                                                 (get-in dfa [:transitions representative ch]))])
                                      alphabet))]))
                   block-ids))
        minimized-accepts
        (into {}
              (keep (fn [[block id]]
                      (when-let [label (get-in dfa [:accepts (block-key block)])]
                        [id label])))
              block-ids)
        trap-id (get state-to-block (:trap dfa))]
    {:start (get state-to-block (:start dfa))
     :trap trap-id
     :alphabet (:alphabet dfa)
     :transitions minimized-transitions
     :accepts minimized-accepts}))

(defn table
  "Return transition rows in stable state and character order."
  [dfa]
  (let [alphabet (sort-by int (:alphabet dfa))]
    (mapv (fn [state]
            {:state state
             :accept (get-in dfa [:accepts state])
             :transitions (into (sorted-map)
                                (map (fn [ch] [(int ch) (get-in dfa [:transitions state ch])])
                                     alphabet))})
          (sort (keys (:transitions dfa))))))

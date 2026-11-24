(ns hw4.main
  "Командная строка HW4: исходник Funny -> IR -> CFG -> оптимизации -> раскраска."
  (:require [clojure.string :as str]
            [hw3.types :as types]
            [hw4.cfg :as cfg]
            [hw4.ir :as ir]
            [hw4.opt :as opt]
            [hw4.regalloc :as regalloc]))

(def usage
  (str "Usage: lein run -m hw4.main <source.funny> [options]\n"
       "  --no-const-prop  --no-algebra  --no-dce   выключить оптимизацию\n"
       "  --cse                                   включить CSE в блоке\n"
       "  --asserts                               компилировать assert/assume в проверки\n"
       "  --dot                                   вывести CFG и граф конфликтов в формате DOT\n"
       "  --k N                                   число регистров (отчёт о spill)\n"))

(def ^:private flags
  {"--no-const-prop" [:const-prop false] "--no-algebra" [:algebra false] "--no-dce" [:dce false]
   "--cse" [:cse true] "--asserts" [:asserts true] "--dot" [:dot true]})

(defn parse-args [args]
  (loop [args args, opts (assoc opt/default-opts :asserts false :dot false), path nil]
    (if-let [a (first args)]
      (if-let [[k v] (flags a)]
        (recur (rest args) (assoc opts k v) path)
        (if (= a "--k")
          (recur (nnext args) (assoc opts :k (parse-long (second args))) path)
          (recur (rest args) opts a)))
      [path opts])))

(defn- report-function [opts before]
  (let [after (opt/optimize-function before opts)
        m0 (opt/metrics before)
        m1 (opt/metrics after)
        alloc0 (regalloc/allocate before (select-keys opts [:k]))
        alloc1 (regalloc/allocate after (select-keys opts [:k]))
        g (cfg/build (:code after))]
    (println (str "=== " (:name before) " ==="))
    (println "--- IR до оптимизаций\n" (ir/function->text before))
    (println "--- IR после оптимизаций\n" (ir/function->text after))
    (println "--- CFG после оптимизаций\n" (cfg/->text g))
    (println "--- граф конфликтов временных (после)\n" (regalloc/graph->text (:graph alloc1) (:colors alloc1)))
    (println "--- метрики")
    (println (format "  инструкций: %d -> %d" (:instructions m0) (:instructions m1)))
    (println (format "  временных:  %d -> %d" (:temps m0) (:temps m1)))
    (println (format "  цветов:     %d -> %d" (:num-colors alloc0) (:num-colors alloc1)))
    (when (:k opts)
      (println (format "  spill при k=%d: %s" (:k opts) (pr-str (:spilled alloc1)))))
    (when (:dot opts)
      (println "--- DOT: CFG\n" (cfg/->dot (:name after) g))
      (println "--- DOT: граф конфликтов\n"
               (regalloc/graph->dot (:name after) (:graph alloc1) (:colors alloc1))))))

(defn -main [& args]
  (let [[path opts] (parse-args args)]
    (if-not path
      (binding [*out* *err*] (print usage) (flush) (System/exit 2))
      (let [{:keys [ast errors]} (types/check-source (slurp path))]
        (if (seq errors)
          (do (binding [*out* *err*]
                (doseq [e errors]
                  (println (str (name (:phase e)) " " (:line e) ":" (:column e) " " (:message e)))))
              (System/exit 1))
          (doseq [f (ir/gen-module ast (select-keys opts [:asserts]))]
            (report-function opts f)))))))

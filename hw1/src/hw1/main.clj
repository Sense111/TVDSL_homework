(ns hw1.main
  (:require [clojure.pprint :as pprint]
            [hw1.automata :as automata]
            [hw1.lexer :as lexer]))

(defn- run-example [compiled {:keys [input expected]}]
  (let [result (try
                 {:tokens (lexer/scan compiled input)}
                 (catch clojure.lang.ExceptionInfo error
                   {:error (.getMessage error)}))
        passed? (= expected (contains? result :tokens))]
    (prn {:input input :expected (if expected :tokens :error)
          :passed? passed? :result result})))

(defn- examples! [compiled]
  (doseq [example [{:input "" :expected true}
                   {:input " \t\r\n" :expected true}
                   {:input "function returns while if else assert assume invariant length"
                    :expected true}
                   {:input "0" :expected true}
                   {:input "01" :expected false}
                   {:input "λ" :expected false}]]
    (run-example compiled example)))

(defn -main [& args]
  (let [command (first args)
        compiled (lexer/compile-lexer)]
    (case command
      "tokens"
      (let [source (if-let [source (second args)] source (slurp *in*))]
        (try
          (pprint/pprint (lexer/scan compiled source))
          (catch clojure.lang.ExceptionInfo error
            (binding [*out* *err*]
              (println (.getMessage error)))
            (System/exit 1))))

      "table"
      (do
        (println "State counts:" (:state-counts compiled))
        (doseq [row (automata/table compiled)]
          (prn row)))

      "examples"
      (examples! compiled)

      (println "Usage: lein run -m hw1.main (tokens [source] | table | examples)"))))

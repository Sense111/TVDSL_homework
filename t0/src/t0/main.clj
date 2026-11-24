(ns t0.main
  (:require [clojure.pprint :as pprint]
            [t0.pipeline :as pipeline]))

(defn -main [& [path what]]
  (if-not path
    (binding [*out* *err*]
      (println "Usage: lein run -m t0.main <source.funny> [tokens|ast|desugared|errors]")
      (System/exit 2))
    (let [result (pipeline/run (slurp path))
          shown (case what
                  "tokens" (:tokens result)
                  "ast" (:ast result)
                  "errors" (:errors result)
                  (select-keys result [:desugared :errors]))]
      (pprint/pprint shown)
      (when (seq (:errors result)) (System/exit 1)))))

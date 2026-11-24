(ns hw3.main
  (:require [clojure.pprint :as pprint]
            [hw3.types :as types]))

(defn -main [& args]
  (if-let [path (first args)]
    (let [result (types/check-source (slurp path))]
      (pprint/pprint result)
      (when (seq (:errors result))
        (System/exit 1)))
    (binding [*out* *err*]
      (println "Usage: lein run -m hw3.main <source.funny>")
      (System/exit 2))))

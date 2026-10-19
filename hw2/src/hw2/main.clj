(ns hw2.main
  (:require [clojure.pprint :as pprint]
            [hw2.parser :as parser]))

(defn -main [& args]
  (if-let [path (first args)]
    (let [result (parser/parse (slurp path))]
      (pprint/pprint result)
      (when (seq (:errors result))
        (System/exit 1)))
    (do
      (binding [*out* *err*]
        (println "Usage: lein run -m hw2.main <source.funny>")
        (System/exit 2)))))

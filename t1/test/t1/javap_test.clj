(ns t1.javap-test
  "Проверка сгенерированных .class инструментом JDK `javap` (-c -v): класс public final,
  методы public static с именами функций Funny, javap завершается без ошибок.
  Файлы пишутся только во временный каталог."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [t1.compile :as c])
  (:import (java.nio.file Files Path)
           (java.nio.file.attribute FileAttribute)))

(defn- javap-output
  "Компилирует `source` в класс funny.<cls>, пишет во временный каталог, запускает javap.
  Возвращает {:exit код :out вывод}."
  [source cls]
  (let [r (c/compile-source source (str "funny." cls))
        dir (Files/createTempDirectory "t1javap" (make-array FileAttribute 0))
        file (.resolve dir (str cls ".class"))]
    (is (nil? (:errors r)) (pr-str (:errors r)))
    (Files/write file ^bytes (:bytes r) (make-array java.nio.file.OpenOption 0))
    (let [javap (str (System/getProperty "java.home") "/bin/javap")
          pkg (Files/createDirectories (.resolve dir "funny") (make-array FileAttribute 0))
          _ (Files/move file (.resolve ^Path pkg (str cls ".class"))
                        (make-array java.nio.file.CopyOption 0))
          pb (doto (ProcessBuilder. ^java.util.List [javap "-c" "-v" "-p" "-cp" (str dir)
                                                      (str "funny." cls)])
               (.redirectErrorStream true))
          p (.start pb)
          out (slurp (.getInputStream p))]
      {:exit (.waitFor p) :out out})))

(deftest javap-accepts-generated-classes
  (doseq [[cls src methods]
          [["FillOk" (slurp "examples/fill_ok.funny") ["sum" "divmod" "fill"]]
           ["SumLike" "function sum(n: int) returns s: int uses i {
                         i = 1; while (i <= n) { s = s + i; i = i + 1; } }" ["sum"]]
           ["Tuple" "function ia(a: int[], n: int) returns r: int, b: int[] { r = n * 2; b = a; b[0] = n; }
                     function ai(a: int[], n: int) returns b: int[], r: int { b = a; b[0] = n; r = length(a); }"
            ["ia" "ai"]]]]
    (let [{:keys [exit out]} (javap-output src cls)]
      (is (= 0 exit) out)
      (is (str/includes? out (str "public final class funny." cls)))
      (is (str/includes? out "ACC_PUBLIC, ACC_FINAL, ACC_SUPER"))
      (is (not (re-find #"(?i)\berror\b|exception:" out)) out)
      (doseq [m methods]
        (is (re-find (re-pattern (str "public static [^\\n]* " m "\\(")) out) (str "метод " m))))))

(deftest javap-shows-tuple-descriptors
  (let [{:keys [out]} (javap-output "function dm(x: int, y: int) returns q: int, r: int { q = x / y; r = x - q * y; }
                                     function mixed() returns n: int, a: int[] { n = 1; }" "Desc")]
    (is (str/includes? out "descriptor: (II)[I") "чистый int-кортеж — int[]")
    (is (str/includes? out "descriptor: ()[Ljava/lang/Object;") "кортеж с массивом — Object[]")))

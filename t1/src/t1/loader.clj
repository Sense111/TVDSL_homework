(ns t1.loader
  "Загрузка сгенерированного байт-кода через clojure.lang.DynamicClassLoader.
  Родитель — загрузчик t1.ImmutableIntArray, поэтому сгенерированный класс видит рантайм.")

(defn define-class
  "Определяет класс `class-name` (с точками: \"funny.Module\") из `bytes` в новом загрузчике."
  ^Class [^String class-name ^bytes bytes]
  (let [loader (clojure.lang.DynamicClassLoader. (.getClassLoader t1.ImmutableIntArray))]
    (.defineClass loader class-name bytes nil)))

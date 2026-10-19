(ns hw1.regex
  (:require [clojure.set :as set]))

(def ^:private ascii-chars (set (map char (range 128))))

(defn- parse-class! [chars pos peek-char take-char class-char fail]
  (take-char) ; [
  (let [negated? (= (peek-char) \^)]
    (when negated? (take-char))
    (loop [members #{}]
      (let [ch (peek-char)]
        (cond
          (nil? ch) (fail "Unclosed character class")
          (= ch \])
          (do
            (take-char)
            (when (and (empty? members) (not negated?))
              (fail "Empty character class"))
            [:class (if negated? (set/difference ascii-chars members) members)])
          :else
          (let [start (class-char)
                range? (and (= (peek-char) \-)
                            (< (inc @pos) (count chars))
                            (not= (nth chars (inc @pos)) \]))]
            (if range?
              (do
                (take-char)
                (let [end (class-char)]
                  (when (> (int start) (int end))
                    (fail "Reversed character range"))
                  (recur (into members (map char (range (int start) (inc (int end))))))))
              (recur (conj members start)))))))))

(defn parse
  "Parse the regular-expression subset used by the HW1 token definitions."
  [pattern]
  (let [chars (vec pattern)
        pos (atom 0)
        size (count chars)
        peek-char (fn [] (when (< @pos size) (nth chars @pos)))
        take-char (fn [] (let [ch (peek-char)] (when ch (swap! pos inc)) ch))
        fail (fn [message]
               (throw (ex-info message {:pattern pattern :index @pos})))
        escaped-char (fn []
                       (let [ch (take-char)]
                         (when-not ch (fail "Incomplete escape"))
                         (case ch
                           \n \newline
                           \r \return
                           \t \tab
                           ch)))
        class-char (fn []
                     (let [ch (take-char)]
                       (cond
                         (nil? ch) (fail "Unclosed character class")
                         (= ch \\) (escaped-char)
                         :else ch)))]
    (letfn [(parse-atom []
              (let [ch (take-char)]
                (cond
                  (nil? ch) (fail "Expected regular-expression atom")
                  (= ch \() (let [node (parse-alt)]
                              (when-not (= (take-char) \))
                                (fail "Unclosed group"))
                              node)
                  (= ch \[) (do (swap! pos dec)
                                (parse-class! chars pos peek-char take-char class-char fail))
                  (= ch \\) [:char (escaped-char)]
                  (contains? #{\) \| \* \+ \? \]} ch)
                  (fail (str "Unexpected metacharacter " ch))
                  :else [:char ch])))
            (parse-repeat []
              (loop [node (parse-atom)]
                (case (peek-char)
                  \* (do (take-char) (recur [:star node]))
                  \+ (do (take-char) (recur [:plus node]))
                  \? (do (take-char) (recur [:optional node]))
                  node)))
            (parse-concat []
              (loop [nodes []]
                (if (or (nil? (peek-char))
                        (contains? #{\) \|} (peek-char)))
                  (case (count nodes)
                    0 [:epsilon]
                    1 (first nodes)
                    [:concat nodes])
                  (recur (conj nodes (parse-repeat))))))
            (parse-alt []
              (loop [branches [(parse-concat)]]
                (if (= (peek-char) \|)
                  (do (take-char) (recur (conj branches (parse-concat))))
                  (if (= 1 (count branches))
                    (first branches)
                    [:alt branches]))))]
      (let [ast (if (zero? size) [:epsilon] (parse-alt))]
        (when (< @pos size)
          (fail (str "Unexpected character " (peek-char))))
        ast))))

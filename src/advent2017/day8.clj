(ns advent2017.day8
  (:require [utils :as utils]))

(def instr-re #"^(\w+) (inc|dec) (-?\d+) if (\w+) (>|<|>=|<=|!=|==) (-?\d+)$")

(defn parse-instr [line]
  (if-let [m (re-matches instr-re line)]
    (->> m
         (rest)
         (map utils/try-parse-int))
    (throw (Exception. (format "Cannot parse %s" line)))))

(defn exec [state instr]
  (let [[reg op n cond-reg c m] instr]
    (if ((case c ">=" >= "<=" <= "<" < ">" > "==" == "!=" not=)
         (get state cond-reg 0)
         m)
      (update state reg (fnil #((case op "inc" + "dec" -) % n) 0))
      state)))

(defn highest-in-register [m]
  (if (empty? m) 0
      (->> m
           (sort-by second >)
           (first)
           (second))))

(defn answer-part1 [lines]
  (->> lines
       (map parse-instr)
       (reduce exec {})
       (highest-in-register)
       ))

(answer-part1 (utils/read-input "2017/day8-example.txt"))
(answer-part1 (utils/read-input "2017/day8.txt"))

(defn answer-part2 [lines]
  (->> lines
       (map parse-instr)
       (reductions exec {})
       (map highest-in-register)
       (reduce max)))

(answer-part2 (utils/read-input "2017/day8-example.txt"))
(answer-part2 (utils/read-input "2017/day8.txt"))

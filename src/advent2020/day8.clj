(ns advent2020.day8
  (:require [utils :as utils]))

(defn parse-instruction [line]
  (let [[_ instr arg] (re-matches #"^(acc|jmp|nop) \+?(-?\d+)$" line)]
    [instr (utils/parse-int arg)]))

(defn parse-program [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-instruction)
       (vec)))

(defn instr-num [program n]
  (mod n (count program)))

(defn step [[program acc line-no]]
  (let [[instr arg] (get program line-no)]
    (case instr
      "nop" [program acc (instr-num program (inc line-no))]
      "jmp" [program acc (instr-num program (+ line-no arg))]
      "acc" [program (+ acc arg) (instr-num program (inc line-no))])))

(defn answer-part1 [filename]
  (reduce
   (fn [seen-lines [_ acc line-no]]
     (if (contains? seen-lines line-no)
       (reduced acc)
       (conj seen-lines line-no)))
   #{}
   (iterate step [(parse-program filename) 0 0])))

(answer-part1 "day8-example.txt")
(answer-part1 "day8.txt")

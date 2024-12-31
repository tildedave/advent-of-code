(ns advent2020.day8
  (:require [utils :as utils]))

(defn parse-instruction [line]
  (let [[_ instr arg] (re-matches #"^(acc|jmp|nop) \+?(-?\d+)$" line)]
    [instr (utils/parse-int arg)]))

(defn parse-program [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map parse-instruction)
       (vec)))

(defn step [[program acc line-no]]
  (let [[instr arg] (get program line-no)]
    (case instr
      "nop" [program acc (inc line-no)]
      "jmp" [program acc (+ line-no arg)]
      "acc" [program (+ acc arg) (inc line-no)])))

(defn final-state [program-seq]
  (reduce
   (fn [seen-lines [program acc line-no]]
     (if (or (contains? seen-lines line-no)
             (nil? (get program line-no)))
       (reduced [acc line-no])
       (conj seen-lines line-no)))
   #{}
   program-seq))

(defn answer-part1 [filename]
  (->> (iterate step [(parse-program filename) 0 0])
       (final-state)
       (first)))

(answer-part1 "day8-example.txt")
(answer-part1 "day8.txt")

(defn answer-part2 [filename]
  (let [program (parse-program filename)]
    (loop [instr-no 0]
      (if (= instr-no (count program))
        (throw (Exception. "Did not find instruction number"))
        (let [[instr _] (get program instr-no)
              state (case instr
                      "nop" (final-state (iterate step [(assoc-in program [instr-no 0] "jmp") 0 0]))
                      "jmp" (final-state (iterate step [(assoc-in program [instr-no 0] "nop") 0 0]))
                      nil)]
          (cond
            (nil? state) (recur (inc instr-no))
            (= (second state) (count program)) (first state)
            :else (recur (inc instr-no))))))))

(answer-part2 "day8-example.txt")
(answer-part2 "day8.txt")

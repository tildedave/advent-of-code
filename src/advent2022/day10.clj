(ns advent2022.day10
  (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day10.txt"))

(defn parse-line [line]
  (cond
    (= line "noop") [:noop]
    (.startsWith line "addx")
    [:addx (utils/parse-int (nth (.split line " ") 1))]))

;; takes a machine state (X register value + currently executing instructions)
;; and converts it into a new state.

(use 'clojure.tools.trace)
(defn process-line [[cycle x-register queue] instr]
  ;; this is the list to reduce
  (let [queue (conj queue (case (first instr)
                            :noop [1 :noop]
                            :addx [2 :addx (second instr)]))
        next-cycle (inc cycle)
        program (get queue 0)
        [cycles-left opcode num] program
        [next-x next-executing]
          (if (= cycles-left 1)
            ;; process it
            (case opcode
              :noop [x-register (subvec queue 1)]
              :addx [(+ x-register num) (subvec queue 1)])
            ;; continue
            [x-register (assoc queue 0 (assoc program 0 (dec cycles-left)))])]
    [next-cycle next-x next-executing]))

(def test-program (map parse-line lines))

(defn process-program [program]
  (loop [cycle 1
         x-register 1
         executing []
         program program
         x-states []]
    (if (and (empty? program)
                 (every? (fn [instr] (= (nth instr 1) :noop)) executing))
      x-states
      (let [[instr & next-program] (or program (list [:noop]))
            [next-cycle next-x next-executing] (process-line [cycle x-register executing] instr)]
        (recur next-cycle next-x next-executing next-program (conj x-states x-register))))))

(let [x-history (process-program test-program)
      indexes (map #(+ % 20) (range 0 220 40))]
  ;; off by 1 for the cycles.
  (reduce + (map (fn [n] (* n (nth x-history (dec n)))) indexes)))

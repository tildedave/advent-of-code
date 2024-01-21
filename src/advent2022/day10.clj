(ns advent2022.day10
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day10.txt"))

(defn parse-line [line]
  (cond
    (= line "noop") [:noop]
    (.startsWith line "addx")
    [:addx (utils/parse-int (nth (.split line " ") 1))]))

;; takes a machine state (X register value + currently executing instructions)
;; and converts it into a new state.

(use 'clojure.tools.trace)
(defn process-line [[cycle x-register queue crt-row] instr]
  ;; this is the list to reduce
  (let [queue (conj queue (case (first instr)
                            :noop [1 :noop]
                            :addx [2 :addx (second instr)]))
        next-cycle (inc cycle)
        program (get queue 0)
        [cycles-left opcode num] program
        crt-index (mod (dec cycle) 40)
        next-crt-row (if (<= (abs (- x-register crt-index)) 1)
                       (assoc crt-row crt-index \#)
                       (assoc crt-row crt-index \.))
        [next-x next-executing]
          (if (= cycles-left 1)
            ;; process it
            (case opcode
              :noop [x-register (subvec queue 1)]
              :addx [(+ x-register num) (subvec queue 1)])
            ;; continue
            [x-register (assoc queue 0 (assoc program 0 (dec cycles-left)))])]
    [next-cycle next-x next-executing next-crt-row]))

(def test-program (map parse-line lines))

(defn process-program [program]
  (loop [cycle 1
         x-register 1
         executing []
         crt-row []
         program program
         x-states []
         crt-rows []]
    (if (and (empty? program)
                 (every? (fn [instr] (= (nth instr 1) :noop)) executing))
      [x-states (conj crt-rows crt-row)]
      (let [[instr & next-program] (or program (list [:noop]))
            carry-over (= (mod (dec cycle) 40) 0)
            crt-rows (if carry-over (conj crt-rows crt-row) crt-rows)
            crt-row (if carry-over  (vec (repeat 40 \space)) crt-row)
            [next-cycle next-x next-executing next-crt-row] (process-line [cycle x-register executing crt-row] instr)]
        (recur next-cycle next-x next-executing next-crt-row next-program (conj x-states x-register) crt-rows)))))

(process-program test-program)

;; answer to part 1
(let [[x-history _] (process-program test-program)
      indexes (map #(+ % 20) (range 0 220 40))]
  ;; off by 1 for the cycles.
  (reduce + (map (fn [n] (* n (nth x-history (dec n)))) indexes)))

;; answer to part 2
(print (string/join "\n" (map string/join (last (process-program test-program)))))

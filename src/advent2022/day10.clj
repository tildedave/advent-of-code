(ns advent2022.day10
  (:require [advent2022.utils :as utils]))

(def lines (utils/read-resource-lines "input/day10-example2.txt"))

(defn parse-line [line]
  (cond
    (= line "noop") [:noop]
    (.startsWith line "addx")
    [:addx (utils/parse-int (nth (.split line " ") 1))]))

;; takes a machine state (X register value + currently executing instructions)
;; and converts it into a new state.
(defn process-line [[x-register executing x-states] program]
  ;; this is the list to reduce
  (let [now-executing (conj executing
                            (case (first program)
                              :noop [1 :noop]
                              :addx [2 :addx (second program)]))
        [next-x next-executing]
        (reduce
         (fn [[x-register next-executing] program]
           (let [[cycles instr] [(dec (first program)) (second program)]]
             (if (= cycles 0)
           ;; process it
               (case instr
                 :noop [x-register next-executing]
                 :addx [(+ x-register (nth program 2)) next-executing])
               [x-register (conj next-executing (assoc program 0 cycles))])))
         [x-register]
         now-executing)]
    [next-x next-executing (conj x-states  next-x)]))


(def test-program (map parse-line lines))

(def result (reduce process-line [1 [] []] test-program))
(nth (nth result 2) 20)

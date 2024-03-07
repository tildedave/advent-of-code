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

(defn process-program [program reporter]
  (loop [cycle 1
         x-register 1
         executing []
         program program]
    (if (and (empty? program)
             (every? (fn [instr] (= (nth instr 1) :noop)) executing))
      ()
      (let [[instr & next-program] (or program (list [:noop]))
            [next-cycle next-x next-executing] (process-line [cycle x-register executing] instr)]
        (reporter cycle x-register)
        (recur next-cycle next-x next-executing next-program)))))

;; answer to part 1
(let [total-signal-strength (atom 0)
      x-state-reporter (fn [cycle x-register]
                         (if (= (mod (- cycle 20) 40) 0)
                           (swap! total-signal-strength + (* cycle x-register))
                           ()))]
  (process-program test-program x-state-reporter)
  (deref total-signal-strength))

(defn crt-reporter [ref cycle x-register]
  (swap! ref
         (fn [crt-rows [cycle x-register]]
           (let [carry-over (= (mod (dec cycle) 40) 0)
                 crt-rows (if carry-over
                            (conj crt-rows (vec (repeat 40 \space)))
                            crt-rows)
                 crt-index (mod (dec cycle) 40)
                 which-row (dec (count crt-rows))]
             (if (<= (abs (- x-register crt-index)) 1)
               (assoc-in crt-rows [which-row crt-index] \#)
               (assoc-in crt-rows [which-row crt-index] \.))))
         [cycle x-register]))

(let [crt-rows (atom [])]
  (process-program test-program (partial crt-reporter crt-rows))
  (print (string/join "\n" (map string/join (deref crt-rows)))))

;; answer to part 2

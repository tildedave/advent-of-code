(ns advent2019.day21
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]))

;; OK so I have a program that works from my previous golang
;; solution.  so we just need to implement it in clojure which
;; should be easy.

(def part1-program
   '("NOT J T"
   "AND T J"
   "AND A T"
   "AND B T"
   "AND C T"
   "NOT T J"
   "AND D J"
   "WALK"))

(def part2-program
  '("NOT J T"
    "AND T J"
    "AND A T"
    "AND B T"
    "AND C T"
    "NOT T J"
    "AND D J"
    "OR E T"
    "OR H T"
    "AND T J"
    "RUN"))

(defn run-springbot [program]
  (let [input (a/chan)
        output (intcode/run-file "2019/day21.txt" input)]
    (println (<!! (intcode/read-until-newline! output)))
    (<!!
     (a/go-loop [program program]
       (if-let [x (first program)]
         (do (<! (intcode/send-string! input x))
             (recur (rest program)))
         nil)))
    (last (<!! (a/into [] output)))))

(println (run-springbot part1-program))
(println (run-springbot part2-program))

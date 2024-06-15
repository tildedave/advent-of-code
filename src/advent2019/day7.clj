(ns advent2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [>!! <!! >! <!]]))

(defn run-amplifier [program phase-sequence]
  (let [inputs (for [_ (range 5)] (a/chan))
        outputs (for [_ (range 5)] (a/chan))]
    (doseq [n (range 5)]
      (a/go
          (intcode/run-program program
                               (nth inputs n)
                               (nth outputs n)))
      (a/thread (>!! (nth inputs n) (nth phase-sequence n)))
      (a/go-loop []
        (if (not= n 4)
          (let [i (<! (nth outputs n))]
            (>! (nth inputs (inc n)) i)
            (recur))
          nil)))
    (>!! (first inputs) 0)
    (<!! (last outputs))))

(run-amplifier "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
               '(4 3 2 1 0))

(defn max-amplifier [program]
  (->> (combo/permutations (range 5))
       (map (partial run-amplifier program))
       (reduce max)))

(max-amplifier "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
(max-amplifier "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")
(max-amplifier "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")
(max-amplifier (intcode/parse-file "2019/day7.txt"))

;; something here isn't right; it sometimes doesn't
;; return the right answer.
(defn run-amplifier-part2 [program phase-sequence]
  (let [inputs (for [_ (range 5)] (a/chan))
        outputs (for [_ (range 5)] (a/chan))]
    (doseq [n (range 5)]
      (let [input (nth inputs n)
            output (nth outputs n)
            phase-number (nth phase-sequence n)]
      (a/go
        (do
          (intcode/run-program program
                               input
                               output)
          (a/close! input)
          (a/close! output)))
      (<!! (a/go (>! input phase-number)))
      (a/go-loop []
        (if (not= n 4)
          (let [i (<! output)]
            (if (nil? i)
              nil
              (do
                (>! (nth inputs (inc n)) i)
                (recur))))
          nil))))
    (>!! (first inputs) 0)
    (<!! (a/go-loop [prev-output nil]
      (let [i (<! (last outputs))]
        (if (nil? i)
          prev-output
          (do
            (>! (first inputs) i)
            (recur i))))))))

(run-amplifier-part2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                     '(9 8 7 6 5))

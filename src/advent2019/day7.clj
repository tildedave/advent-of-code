(ns advent2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [>!! <!! >! <!]]))

(defn run-amplifier [program phase-sequence]
  (let [system-input (a/chan)
        system-output
        (loop [n 0
               input system-input]
          (if (= n 5)
            input
            (let [next-input (intcode/run-program program input)]
              (>!! input (nth phase-sequence n))
              (recur (inc n) next-input))))]
    (>!! system-input 0)
    (last (<!! (a/into [] system-output)))))

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

;; changing intcode so that it closes the channels seems to have caused this
;; to infinite loop; let's figure out why (later)
(defn run-amplifier-part2 [program phase-sequence]
  (let [system-input (a/chan)
        system-output
        (loop [n 0
               input system-input]
          (if (= n 5)
            input
            (let [next-input (intcode/run-program program input)]
              (>!! input (nth phase-sequence n))
              (recur (inc n) next-input))))]
    (>!! system-input 0)
    (<!! (a/go-loop [last-input nil]
           (let [v (<! system-output)]
             (if (nil? v)
               last-input
               (do
                 (a/put! system-input v)
                 (recur v))))))))

(println
 (run-amplifier-part2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                      '(9 8 7 6 5)))

(defn max-amplifier-part2 [program]
  (->> (combo/permutations (range 5))
       (map #(map (partial + 5) %))
       (map (partial run-amplifier-part2 program))
       (reduce max)))

;; part 2
(max-amplifier-part2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
(println (max-amplifier-part2 (intcode/parse-file "2019/day7.txt")))

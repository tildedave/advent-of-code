(ns advent2020.day9
  (:require [utils :as utils]
            [clojure.set :as set]))

;; OK this is more complex.
;; still, not too hard.

(defn answer-part1 [filename prefix-length]
  (let [[preamble rest] (->> (utils/read-input (format "2020/%s" filename))
                             (map utils/parse-long)
                             (split-at prefix-length)
                             (map vec))]
    (loop [preamble preamble rest rest]
      (if (empty? rest) nil
          (let [x (first rest)
                residues (map (fn [n] (- x n)) preamble)]
            (if (empty? (set/intersection (set residues) (set preamble)))
              x
              (recur
               (conj (subvec preamble 1) (first rest))
               (subvec rest 1))))))))

(answer-part1 "day9-example.txt" 5)
(answer-part1 "day9.txt" 25)

(defn answer-part2 [filename prefix-length]
  (let [lines (->> (utils/read-input (format "2020/%s" filename))
                   (map utils/parse-long)
                   (vec))
        target (answer-part1 filename prefix-length)
        [start end] (loop [start 0
                           end 1
                           total-sum (get lines 0)]
                      (cond
                        (= total-sum target) [start end]
                        (< total-sum target)
                        (recur start
                               (inc end)
                               (+ total-sum (get lines (inc end))))
                        (> total-sum target)
                        (recur (inc start)
                               end
                               (- total-sum (get lines start)))))]
    (->> (range start end)
         (map #(get lines %))
         ((fn [s] [(reduce min s) (reduce max s)]))
         (reduce +)
         )))

(answer-part2 "day9-example.txt" 5)
(answer-part2 "day9.txt" 25)

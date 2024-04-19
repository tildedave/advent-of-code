(ns advent2015.day24
  (:require [clojure.math.combinatorics :as combo]
            [utils :as utils]))

(defn answer [numbers divisor]
  (let [target (/ (reduce + numbers) divisor)]
    (loop [n 1]
      (let [valid-first-partition
            (->> (combo/combinations numbers n)
                 (filter #(= target (reduce + %)))
                 (sort-by #(reduce * %))
                 (first))]
        (if (empty? valid-first-partition)
          (recur (inc n))
          (reduce * valid-first-partition))))))

(answer [1 2 3 4 5 7 8 9 10 11] 3)
(answer (map utils/parse-int (utils/read-input "2015/day24.txt")) 3)
(answer [1 2 3 4 5 7 8 9 10 11] 4)
(answer (map utils/parse-int (utils/read-input "2015/day24.txt")) 4)

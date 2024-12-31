(ns advent2015.day17
  (:require [clojure.math.combinatorics :as combo]
            [utils :as utils]))

(defn count-possibilities [nums target]
  (let [duplicates (->> nums
                        (map #(hash-map % 1))
                        (reduce (partial merge-with +))
                        (remove (fn [[x n]] (= n 1)))
                        (map #(apply hash-map %))
                        (into {}))]
    (->> (combo/subsets nums)
         (filter #(= (reduce + %) target))
         (map #(let [[n _] (reduce (fn [[acc already-counted] v]
                                     (if (contains? already-counted v)
                                       [(/ acc (get duplicates v 1))
                                        already-counted]
                                       [(* (get duplicates v 1) acc)
                                        (conj already-counted v)]))
                                   [1 #{}] %)]
                 [n %])))))

(defn answer-part1 [nums target]
  (->> (count-possibilities nums target)
       (map first)
       (reduce +)))

(answer-part1 [20 15 10 5 5] 25)

(answer-part1 (->> (utils/read-input "2015/day17.txt")
                   (map utils/parse-int))
              150)

(defn answer-part2 [nums target]
  (->> (count-possibilities nums target)
       (group-by #(count (second %)))
       (seq)
       (sort-by first)
       (first)
       (second)
       (map first)
       (reduce +)
       ))

(answer-part2  [20 15 10 5 5] 25)

(answer-part2 (->> (utils/read-input "2015/day17.txt")
                   (map utils/parse-int))
              150)

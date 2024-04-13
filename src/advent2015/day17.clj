(ns advent2015.day17
  (:require [clojure.math.combinatorics :as combo]
            [utils :as utils]))

(defn answer-part1 [nums]
  (let [duplicates (->> nums
                        (map #(hash-map % 1))
                        (reduce (partial merge-with +))
                        (remove (fn [[x n]] (= n 1)))
                        (map #(apply hash-map %))
                        (into {}))]
    (println nums)
    (->> (combo/subsets nums)
         (filter #(= (reduce + %) 25))
         (map #(reduce (fn [[acc already-counted] v]
                         (if (contains? already-counted v)
                           [(/ acc (get duplicates v 1))
                            already-counted]
                           [(* (get duplicates v 1) acc)
                            (conj already-counted v)]))
                       [1 #{}] %))
         (map first)
         (reduce +))))

(answer-part1 [20 15 10 5 5])

(answer-part1 (->> (utils/read-input "2015/day17.txt")
                   (map utils/parse-int)))

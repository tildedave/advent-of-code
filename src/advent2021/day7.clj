(ns advent2021.day7
  (:require [advent2021.utils :as utils]))

;; we will just brute force it. >:-)

(defn parse-lines [lines]
  (->> (.split (first lines)  ",")
       (map utils/parse-int)
       (sort)
       (vec)))

(defn fuel-amount [num-vec num]
  (->> num-vec
       (map #(- % num))
       (map abs)
       (reduce +)))

(defn fuel-amount-part2 [num-vec num]
  (->> num-vec
       (map #(- % num))
       (map abs)
       (map #(quot (* % (inc %)) 2))
       (reduce +)))

(defn f [num] #(let [n (- % num)] (quot (* n (inc n)) 2)))

((f 0) 3)


(defn answer-part1 [lines]
  (let [v (parse-lines lines)]
    ;; I guess this works because the answer must be at a
    ;; submarine.
    (reduce min (map (partial fuel-amount v) v))))

(answer-part1 (utils/read-resource-lines "input/day7-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day7.txt"))

(defn answer-part2 [lines]
  (let [v (parse-lines lines)
        best-sub
    ;; so this is the minimum submarine, now we need to potentially see
    ;; if we can do better.
        (->> v
             (map (partial fuel-amount-part2 v))
             (map-indexed vector)
             (sort-by second)
             (first)
             (first))
        best-pos (loop [pos best-sub]
      ;; look left, look right, if one of them is less, recur on that.
                   (let [next-pos (->> [(dec pos) pos (inc pos)]
                                       (map #(vector % (fuel-amount-part2 v %)))
                                       (sort-by second)
                                       (first)
                                       (first))]
                     (if (= pos next-pos) pos
                         (recur next-pos))))]
    (fuel-amount-part2 v best-pos)))

(answer-part2 (utils/read-resource-lines "input/day7-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day7.txt"))

    (let [v (parse-lines (utils/read-resource-lines "input/day7-example.txt"))]
      (fuel-amount-part2 v 5))

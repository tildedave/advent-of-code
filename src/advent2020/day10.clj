(ns advent2020.day10
  (:require [utils :as utils]))

(get (vector 1 2 3) 1)

(defn count-differences [sorted-vec]
  (loop
   [last-voltage 0
    idx 0
    difference-count {1 0 2 0 3 0}]
    (if (= idx (count sorted-vec))
      (update difference-count 3 inc)
      (let [curr-voltage (long (get sorted-vec idx))]
        (recur
         curr-voltage
         (inc idx)
         (update difference-count (- curr-voltage last-voltage) inc))))))

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map utils/parse-int)
       (sort)
       (vec)
       (count-differences)
       (filter (fn [[k _]] (or (= k 1) (= k 3))))
       (map second)
       (reduce *)))

(answer-part1 "day10-example.txt")
(answer-part1 "day10-example2.txt")
(answer-part1 "day10.txt")

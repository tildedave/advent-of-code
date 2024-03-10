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

(defn last-value [sorted-vec]
  (+ (get sorted-vec (dec (count sorted-vec))) 3))

(defn next-idxs [sorted-vec idx last-value]
  (let [v (if (= idx -1) 0 (get sorted-vec idx))]
    (->> '(1 2 3)
         (map (partial + idx))
         (filter #(<= % (count sorted-vec)))
         (filter #(<= (- (get sorted-vec % last-value) v) 3)))))

(defn count-combinations [sorted-vec]
  (let [lv (last-value sorted-vec)
        f (memoize (fn [f idx]
            (if (= idx (count sorted-vec)) 1
              (->> (next-idxs sorted-vec idx lv)
                   (map (partial f f))
                   (reduce +)))))
        f (partial f f)]
    (f -1)))

(defn answer-part2 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map utils/parse-int)
       (sort)
       (vec)
       (count-combinations)))

(answer-part2 "day10-example.txt")
(answer-part2 "day10-example2.txt")
(answer-part2 "day10.txt")

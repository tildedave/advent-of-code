(ns advent2023.day9
  (:require [utils :as utils]))

(defn differences [l]
  (mapv - (rest l) l))

(defn extrapolate [l]
  (->> l
       (iterate differences)
       (take-while #(not (every? zero? %)))
       (map last)
       (reduce +)))

(extrapolate [0 3 6 9 12 15])
(extrapolate [1   3   6  10  15  21])

;; part 1
(->> (utils/read-input "2023/day9.txt")
     (map utils/parse-number-list)
     (map extrapolate)
     (reduce +))

;; it's possible that there's more of a closed form solution
;; to this, but this is simple enough.
(defn extrapolate-backwards [l]
  (loop [num-list (->> l
       (iterate differences)
       (take-while #(not (every? zero? %)))
       (map first)
       (reverse))
         last-value 0]
    (if (empty? num-list)
      last-value
      (recur (rest num-list) (- (first num-list) last-value)))))

(extrapolate-backwards [10 13 16 21 30 45])

;; part 2
(->> (utils/read-input "2023/day9.txt")
     (map utils/parse-number-list)
     (map extrapolate-backwards)
     (reduce +))

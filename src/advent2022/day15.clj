(ns advent2022.day15
  (:require [utils :as utils]))

(def lines (utils/read-resource-lines "input/day15.txt"))
(def sensor-re #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def coords
  (map
   #(let [[sx sy bx by] (->> %
                             (re-matches sensor-re)
                             (rest)
                             (map utils/parse-int))]
      [[sx sy] [bx by]])
   lines))

(def sensors-with-distances
  (map #(vector (first %) (apply manhattan-distance %)) coords))

(defn impossible-in-range [y [[sx sy] beacon-distance]]
  (let [diff (- beacon-distance (manhattan-distance [sx y] [sx sy]))]
    (if (< diff 0)
      []
      [[(- sx diff) y] [(+ sx diff) y]])))

(manhattan-distance [8 7] [8 1])
(impossible-in-range 10 [[8 7] 9])

(defn interval-overlap? [r1 r2]
  ;; return if [start end] is inside [interval-start interval-end]
  ;; took this from day4
  (not (or (< (second r1) (first r2))
           (> (first r1) (second r2)))))

(defn interval-merge [r1 r2]
  [(min (first r1) (first r2)) (max (second r1) (second r2))])

(interval-merge [2 5] [-2 2])

;; perf of this is awful but whatever
(defn interval-merge-in [intervals r]
  (loop [merge-in r
         intervals intervals]
    (let [[x] (filter (partial interval-overlap? merge-in) intervals)]
      (if (nil? x)
        (conj intervals merge-in)
        (recur (interval-merge x merge-in)
               (filter #(not= x %) intervals))))))

;; answer to part 1
(->> sensors-with-distances
     (map (partial impossible-in-range 2000000))
     (filter #(not= [] %))
     (mapv #(mapv first %))
     (reduce interval-merge-in [])
     (map #(abs (- (second %) (first %))))
     (reduce + 0))

;; brute force part 2!
(loop [y 0]
  (let [intervals (->> sensors-with-distances
                       (map (partial impossible-in-range y))
                       (filter #(not= [] %))
                       (mapv #(mapv first %))
                       (reduce interval-merge-in []))]
    (if (> (count intervals) 1)
      (println y intervals)
      (do
        (println y intervals)
        (recur (inc y))))))

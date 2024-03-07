(ns advent2021.day11
  (:require [advent2021.utils :as utils]
            [clojure.set :as set]))


(defn parse-grid [lines]
  (->> lines
       (map seq)
       (map #(mapv (fn [ch] (utils/parse-int (str ch))) %))
       (vec)))

(defn bounds [grid]
  [(count (first grid)) (count grid)])

(defn flashing-coords [grid]
  (let [[xmax ymax] (bounds grid)]
    (->> (for [x (range 0 xmax)
               y (range 0 ymax)]
           (if (> (get-in grid [y x]) 9)
             [x y]
             nil))
         (remove nil?)
         (set))))

(defn out-of-bounds? [grid [x y]]
  (or (< y 0)
      (>= y (count grid))
      (< x 0) (>= x (count (first grid)))))

(defn increase-energy [grid]
  (->> grid
       (mapv #(mapv inc %))))

(defn flash [grid [x y]]
  ;; dx dy etc
  (reduce
   (fn [grid [x y]]
     (update-in grid [y x] inc))
   grid
   (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]
                       [-1 -1] [1 -1] [-1 1] [1 1]]]
          [(+ x dx) (+ y dy)])
        (remove (partial out-of-bounds? grid)))))

(defn tick [grid]
  (let [grid (increase-energy grid)]
  (loop [grid grid
         flashed-octopi #{}]
    (let [flashing-coords (-> (flashing-coords grid)
                              (set/difference flashed-octopi))]
    (if (empty? flashing-coords)
      [(reduce (fn [grid [x y]]
                 (assoc-in grid [y x] 0))
                 grid flashed-octopi)
       flashed-octopi]
        (recur
         (reduce flash grid flashing-coords)
         (set/union flashed-octopi flashing-coords)))))))

(defn flash-sequence [grid]
  (let [[next-grid flashed-octopi] (tick grid)]
    (lazy-seq (cons (count flashed-octopi) (flash-sequence next-grid)))))

(defn answer-part1 [lines]
  (->> lines
       (parse-grid)
       (flash-sequence)
       (take 100)
       (reduce +)))

(answer-part1 (utils/read-resource-lines "input/day11-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day11.txt"))

(defn answer-part2 [lines]
  (let [grid (->> lines (parse-grid))
        [xmax ymax] (bounds grid)]
    (->> (flash-sequence grid)
         (map-indexed vector)
         (filter #(= (second %) (* xmax ymax)))
         (first)
         (first)
         (inc))))

(answer-part2 (utils/read-resource-lines "input/day11-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day11.txt"))

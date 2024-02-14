(ns advent2021.day9
  (:require [advent2021.utils :as utils]
            [clojure.set :as set]))

(defn parse-grid [lines]
  (->> lines
       (map seq)
       (map #(mapv (fn [ch] (utils/parse-int (str ch))) %))
       (vec)))

(parse-grid (utils/read-resource-lines "input/day9-example.txt"))

(defn out-of-bounds? [grid [x y]]
  (or (< y 0)
      (>= y (count grid))
      (< x 0) (>= x (count (first grid)))))

(out-of-bounds?
 (parse-grid (utils/read-resource-lines "input/day9-example.txt"))
 [-1 0])

(defn neighbors [grid [x y]]
  (remove
   nil?
   (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]]
     (let [[nx ny] [(+ x dx) (+ y dy)]]
       (if (out-of-bounds? grid [nx ny])
         nil
         [nx ny])))))

(defn coords [grid]
  (let [xmax (count (first grid))
        ymax (count grid)]
   (for [x (range 0 xmax)
         y (range 0 ymax)]
     [x y])))

(defn low-points [grid]
  (remove nil?
          (for [[x y] (coords grid)]
            (let [height (get-in grid [y x])]
              (if (->> (neighbors grid [x y])
                       (map #(< height (get-in grid [(second %) (first %)])))
                       (every? true?))
                [x y]
                nil)))))

(defn answer-part1 [lines]
  (let [grid (parse-grid lines)]
    (->> (low-points grid)
         (map #(inc (get-in grid [(second %) (first %)])))
         (reduce +))))

(answer-part1 (utils/read-resource-lines "input/day9-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day9.txt"))

;; basins are basically a component connection problem.
;; we can BFS from any location to find its components.
;; we start from the low points

(defn higher-neighbors [grid [x y]]
  (let [height (get-in grid [y x])]
    (->> (neighbors grid [x y])
         (filter (fn [[nx ny]]
                   (and (< height (get-in grid [ny nx]))
                        (not= (get-in grid [ny nx]) 9)))))))

(defn search-from [grid [x y]]
  (loop [[queue seen result] [[[x y]] #{[x y]} [[x y]]]]
    (if (empty? queue)
      result
      (let [curr (first queue)
            queue (subvec queue 1)]
        (recur
         (reduce
          (fn [[queue seen result] neighbor]
            (if (not (contains? seen neighbor))
              [(conj queue neighbor)
               (conj seen neighbor)
               (conj result neighbor)]
              [queue seen result]))
          [queue seen result]
          (higher-neighbors grid curr)))))))

(search-from (parse-grid (utils/read-resource-lines "input/day9-example.txt"))
             [1 0])

(defn answer-part2 [lines]
  (let [grid (parse-grid lines)]
    (->>
     (low-points grid)
     (map (partial search-from grid))
     (map count)
     (sort)
     (reverse)
     (take 3)
     (reduce *))))

(answer-part2 (utils/read-resource-lines "input/day9-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day9.txt"))

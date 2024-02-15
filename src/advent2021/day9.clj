(ns advent2021.day9
  (:require [advent2021.utils :as utils]
            [advent2021.grid :as grid]
            [clojure.set :as set]))

(defn low-points [grid]
  (remove nil?
          (for [[x y] (grid/coords grid)]
            (let [height (get-in grid [y x])]
              (if (->> (grid/neighbors grid [x y])
                       (map #(< height (get-in grid [(second %) (first %)])))
                       (every? true?))
                [x y]
                nil)))))

(def parse-sq (fn [ch] (utils/parse-int (str ch))))

(defn answer-part1 [lines]
  (let [grid (grid/parse lines parse-sq)]
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
    (->> (grid/neighbors grid [x y])
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

(defn answer-part2 [lines]
  (let [grid (grid/parse lines parse-sq)]
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

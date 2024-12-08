(ns advent2024.day8
  (:require
   [grid :as grid]
   [clojure.set :as set]
   [utils :as utils]))

(def example-lines
  '("............"
    "........0..."
    ".....0......"
    ".......0...."
    "....0......."
    "......A....."
    "............"
    "............"
    "........A..."
    ".........A.."
    "............"
    "............"))

(grid/parse example-lines)

(defn antennae-coords [grid]
  (->> (grid/coords grid)
       (filter #(not= (grid/at grid %) \.))
       (map #(hash-map (grid/at grid %) #{%}))
       (reduce (partial merge-with set/union))))

(antennae-coords (grid/parse example-lines))

(defn antinodes [grid coord-list]
  (->>
   (for [x coord-list
         y coord-list]
     (if (= x y)
       nil
       (mapv - y (mapv - x y))))
   (remove nil?)
   (remove (partial grid/out-of-bounds? grid))
   (set)))

(defn total-antinodes-part1 [lines]
  (let [grid (grid/parse lines)]
    (as-> (antennae-coords grid) v
      (update-vals v (partial antinodes grid))
      (vals v)
      (reduce set/union v)
      (count v))))

(total-antinodes-part1 (utils/read-input "2024/day8.txt"))

(let [grid (grid/parse example-lines)]
  (antinodes
   grid
   ((antennae-coords grid) \A)))

(def second-example
  '("T....#...."
    "...T......"
    ".T....#..."
    ".........#"
    "..#......."
    ".........."
    "...#......"
    ".........."
    "....#....."
    ".........."))

(defn antinodes-part2 [grid coord-list]
  (->>
   (for [x coord-list
         y coord-list]
     (let [offset (mapv - x y)]
       (if (= x y)
         #{y}
         (loop
          [curr y
           result #{}]
           (if (grid/out-of-bounds? grid curr)
             result
             (recur (mapv - curr offset) (conj result curr)))))))
   (reduce set/union)
   ))

(let [grid (grid/parse second-example)]
  (antinodes-part2
   grid
   ((antennae-coords grid) \T)))

(let [grid (grid/parse example-lines)]
  (antinodes-part2
   grid
   ((antennae-coords grid) \0)))

(defn total-antinodes-part2 [lines]
  (let [grid (grid/parse lines)]
    (as-> (antennae-coords grid) v
      (update-vals v (partial antinodes-part2 grid))
      (vals v)
      (reduce set/union v)
      (count v))))

(total-antinodes-part2 example-lines)
(total-antinodes-part2 (utils/read-input "2024/day8.txt"))

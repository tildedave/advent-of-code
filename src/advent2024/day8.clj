(ns advent2024.day8
  (:require
   [grid :as grid]
   [clojure.set :as set]))

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
       (mapv - x (mapv - x y))))
   (remove nil?)
   (remove (partial grid/out-of-bounds? grid))
   (set)))

(let [grid (grid/parse example-lines)]
  (as-> (antennae-coords grid) v
    (update-vals v (partial antinodes grid))
    (vals v)
    (reduce set/union v)))

(let [grid (grid/parse example-lines)]
  (antinodes
   grid
   ((antennae-coords grid) \A)))

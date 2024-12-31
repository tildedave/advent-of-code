(ns advent2023.day17
  (:require [grid :as grid]
            [utils :as utils]
            [graph :as graph]))

;; so this one is just dijkstra search

(def example-grid
  '("2413432311323"
    "3215453535623"
    "3255245654254"
    "3446585845452"
    "4546657867536"
    "1438598798454"
    "4457876987766"
    "3637877979653"
    "4654967986887"
    "4564679986453"
    "1224686865563"
    "2546548887735"
    "4322674655533"))

(defn parse-heat-grid [lines]
  (grid/parse lines #(utils/parse-int (str %))))

(defn turn [facing turn-direction]
  (if (= turn-direction :forward)
    facing
    (case facing
      :north (case turn-direction
               :left  :west
               :right :east)
      :east (case turn-direction
              :left :north
              :right :south)
      :west (case turn-direction
              :left :south
              :right :north)
      :south (case turn-direction
               :left :east
               :right :west))))

(def facing-map {:north [0 -1] :south [0 1] :west [-1 0] :east [1 0]})

(defn move [coord facing]
  (mapv + coord (facing-map facing)))

(defn move-direction [[coord facing num-steps] turn-direction]
  [(move coord (turn facing turn-direction))
   (turn facing turn-direction)
   (if (= turn-direction :forward) (inc num-steps) 1)])

(defn coord-out-of-bounds? [grid [coord _ _]]
  (grid/out-of-bounds? grid coord))

(defn neighbors-part1 [grid [coord facing num-steps]]
  (->>
   (map
    (partial move-direction [coord facing num-steps])
    (if (= num-steps 3) [:left :right]
        [:left :right :forward]))
   (remove (partial coord-out-of-bounds? grid))))

;; part 1
(let [grid (parse-heat-grid (utils/read-input "2023/day17.txt"))
      bottom-corner (mapv dec (grid/bounds grid))]
  (->>
   (graph/dijkstra-search
   [[0 0] :east 0]
   (partial neighbors-part1 grid)
   (fn [& args] false)
   (fn [current neighbor]
     (grid/at grid (first neighbor))))
   (filter (fn [[[coord _] _]] (= coord bottom-corner)))
   (sort-by second)
   (first)
   (last)))

(defn neighbors-part2 [grid [coord facing num-steps]]
  (->>
   (if (< num-steps 4)
    (list [(move coord facing) facing (inc num-steps)])
    (map (partial move-direction [coord facing num-steps])
         (if (= num-steps 10) [:left :right]
             [:left :right :forward])))
    (remove (partial coord-out-of-bounds? grid))))

(def unfortunate-example
  '("111111111111"
    "999999999991"
    "999999999991"
    "999999999991"
    "999999999991"))

;; part2
(let [grid (parse-heat-grid (utils/read-input "2023/day17.txt"))
      bottom-corner (mapv dec (grid/bounds grid))]
  (->>
   (graph/dijkstra-search
    [[0 0] :east 0]
    (partial neighbors-part2 grid)
    (fn [& args] false)
    (fn [current neighbor]
      (grid/at grid (first neighbor))))
   (filter (fn [[[coord _] _]] (= coord bottom-corner)))
   (filter (fn [[[_ _ num-steps] _]] (>= num-steps 4)))
   (sort-by second)
   (first)
   (last)))

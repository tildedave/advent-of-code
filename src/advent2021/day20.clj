(ns advent2021.day20
   (:require [advent2021.utils :as utils]
             [clojure.string :as string]
             [advent2021.grid :as grid]))

 (into {} (map-indexed vector [1 2 3]))

 (defn parse-input [filename]
   (let [[[enhancement-algo] image-lines]
         (->> (utils/read-input filename)
              (partition-by (partial = ""))
              (remove (partial = (list ""))))]
     [enhancement-algo
      (-> (->> (grid/parse image-lines identity)
               (map-indexed vector)
               (into {}))
          (update-vals #(into {} (map-indexed vector %))))]))


 (for [dy (range -1 2)
       dx (range -1 2)]
   [dx dy])

 (defn algo-index [grid background [x y]]
   (->> (for [dy (range -1 2)
              dx (range -1 2)]
          [dx dy])
        (map (fn [[dx dy]] (get-in grid [(+ y dy) (+ x dx)] background)))
        (map #(case % \. 0 \# 1))
        (string/join)
        (#(Long/parseLong % 2))))

;; so this is the standard coords, just with a +2 boundary.
;; we also need the grid to be an assoc map so we can go infinite.
 (defn min-max [nums]
   [(reduce min nums) (reduce max nums)])

 (defn coords [grid]
  ;; we will assume the grid is square for my convenience.
   (let [[min max] (min-max (keys grid))]
     (for [x (range (- min 2) (+ (inc max) 2))
           y (range (- min 2) (+ (inc max) 2))]
       [x y])))

(defn step [enhancement-algo [grid background]]
  [(->> (coords grid)
       (map #(vector % (algo-index grid background %)))
       ;; so this is the updates that have to run
       ;; we can just reduce them.
       (reduce (fn [grid [[x y] idx]]
                 (assoc-in grid [y x] (.charAt enhancement-algo idx)))
               grid))
        (.charAt enhancement-algo (case background \. 0 \# 511))
   ])

(def example-grid (second (parse-input "day20-example.txt")))

(defn step-seq [filename]
  (let [[enhancement-algo grid] (parse-input filename)]
    (iterate #(step enhancement-algo %) [grid \.])))

(nth (step-seq "day20-example.txt") 2)

(nth (step-seq "day20.txt") 2)

(defn answer-part1 [filename]
  (->> (nth (step-seq filename) 2)
       (first)
       (vals)
       (mapcat vals)
       (filter (partial = \#))
       (count)))

(answer-part1 "day20-example.txt")
(answer-part1 "day20.txt")

(defn answer-part2 [filename]
  (->> (nth (step-seq filename) 50)
       (first)
       (vals)
       (mapcat vals)
       (filter (partial = \#))
       (count)))

(answer-part2 "day20-example.txt")
(println (answer-part2 "day20.txt"))

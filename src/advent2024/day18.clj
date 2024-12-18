(ns advent2024.day18
  (:require
    [grid :as grid]
    [utils :as utils]
    [graph :as graph]))

;; A* search
;; I would expect that the bits are going to start falling in real-time in part 2

(def example-grid (grid/make 7 7 \.))
(def example-coords (utils/read-input "2024/day18-example.txt"))
(defn parse-coords [lines] (map #(vec (utils/str->nums %)) lines))
(parse-coords example-coords)
(defn coords-fall [grid coords-list]
  (reduce (fn [grid coords] (grid/assoc grid coords \O)) grid coords-list))

(let [example (coords-fall example-grid (take 12 (parse-coords example-coords)))]
  (graph/a*-search
   [0 0]
   #(= % [6 6])
   (fn [coords] (grid/neighbors example coords grid/cardinal-directions #(= % \O)))
   (fn [_] 10)
   (fn [_ _] 1)))

(defn answer-part1 [grid start end falling-coords]
(let [grid (coords-fall grid falling-coords)]
  (first
   (graph/a*-search
   start
   #(= % end)
   (fn [coords] (grid/neighbors grid coords grid/cardinal-directions #(= % \O)))
   (fn [_] 10)
   (fn [_ _] 1)))))

(answer-part1 (grid/make 7 7 \.) [0 0] [6 6] )

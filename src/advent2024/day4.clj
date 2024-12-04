(ns advent2024.day4
  (:require
   [grid :as grid]
   [utils :as utils]))

(def example-lines
  '("MMMSXXMASM"
    "MSAMXMSMSA"
    "AMXSXMAAMM"
    "MSAMASMSMX"
    "XMASAMXAMM"
    "XXAMMXXAMA"
    "SMSMSASXSS"
    "SAXAMASAAA"
    "MAMMMXMMMM"
    "MXMXAXMASX"))

(grid/parse example-lines)

(defn coords-in-direction [grid coords direction length]
  (loop
   [coords coords
    length length
    result []]
   (cond
    (zero? length) result
    (grid/out-of-bounds? grid coords) nil
    :else (recur (mapv + coords direction) (dec length) (conj result coords)))))

(coords-in-direction
 (grid/parse example-lines)
 [0 0]
 [0 1]
 4)

(defn answer-part1 [lines]
  (let [grid (grid/parse lines)]
  (->>
   (for [coords (grid/coords grid)
         dir grid/all-directions]
     (coords-in-direction grid coords dir 4))
   (remove nil?)
   (map (partial map (partial grid/at grid)))
   (filter #(= % '(\X \M \A \S)))
   (count))))

(defn is-x? [grid coords]
  ;; we're an A at our coord
  ;; in one direction, two are Ms, the rest of Ss
  ;; we can't have 1 1 | -1 1 both Ms
  (let [neighbors (map #(vector % (mapv + coords %)) '([-1 -1] [-1 1] [1 -1] [1 1]))
        neighbor-coords (map second neighbors)]
    (cond
      (not= (grid/at grid coords) \A) false
      (seq (filter (partial grid/out-of-bounds? grid) neighbor-coords)) false
      :else
      (contains?
       #{{[-1 -1] \M [1 -1] \M [-1 1] \S [1 1] \S}
         {[-1 -1] \S [1 -1] \S [-1 1] \M [1 1] \M}
         {[-1 -1] \M [1 -1] \S [-1 1] \M [1 1] \S}
         {[-1 -1] \S [1 -1] \M [-1 1] \S [1 1] \M}}
       (reduce into {} (map (fn [[d coords]] {d (grid/at grid coords)}) neighbors))))))

(defn answer-part2 [lines]
  (let [grid (grid/parse lines)]
    (->>
     (grid/coords grid)
     (filter (partial is-x? grid))
     (count))))

(answer-part1 example-lines)
(answer-part2 example-lines)
(answer-part1 (utils/read-input "2024/day4.txt"))
(answer-part2 (utils/read-input "2024/day4.txt"))

(ns advent2023.day3
  (:require [grid :as grid]
            [utils :as utils]
            [clojure.string :as string]))

(defn is-number? [ch]
  (case ch
    (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) true
    false))

(defn number-contained-at [grid [x y]]
  (let [startx (loop [x x]
                 (cond
                   (= x -1) 0
                   (not (is-number? (grid/at grid [x y]))) (inc x)
                   :else (recur (dec x))))
        endx (loop [x x]
               (cond
                 (= x (count (first grid))) x
                 (not (is-number? (grid/at grid [x y]))) (dec x)
                 :else (recur (inc x))))]
    (->>
     (for [x (range startx (inc endx))]
       [x y])
     (map #(grid/at grid %))
     ((fn [seq] (if (empty? seq) nil (utils/parse-int (string/join seq))))))))

(def example-grid (grid/parse
                   '("467..114.."
                     "...*......"
                     "..35..633."
                     "......#..."
                     "617*......"
                     ".....+.58."
                     "..592....."
                     "......755."
                     "...$.*...."
                     ".664.598..")))

(number-contained-at example-grid [5 0])

(let [not-symbols #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.}]
  (defn symbol-coords [grid]
    (->> (grid/coords grid)
         (filter #(not (contains? not-symbols (grid/at grid %)))))))

(symbol-coords example-grid)

(defn adjacent-parts [grid c]
  (->> (grid/neighbors grid c grid/all-directions)
       (map (partial number-contained-at grid))
       (remove nil?)
       (set)))

(defn part-numbers [grid]
  (->> (symbol-coords grid)
       (mapcat (partial adjacent-parts grid))))

;; part 1
(reduce + (part-numbers example-grid))
(reduce + (part-numbers (grid/parse-file "2023/day3.txt")))

(defn candidate-gear-coords [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \*))))

(defn answer-part2 [grid]
  (->> (candidate-gear-coords grid)
       (map
        (fn [c]
          (let [nearby-parts (adjacent-parts grid c)]
            (if (= (count nearby-parts) 2)
              (reduce * nearby-parts)
              0))))
       (reduce +)))

(answer-part2 example-grid)
(answer-part2 (grid/parse-file "2023/day3.txt"))

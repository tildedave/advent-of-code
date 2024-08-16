(ns advent2023.day21
    (:require [grid :as grid]))

(def example-grid
  '("..........."
    ".....###.#."
    ".###.##..#."
    "..#.#...#.."
    "....#.#...."
    ".##..S####."
    ".##..#...#."
    ".......##.."
    ".##.#.####."
    ".##..##.##."
    "..........."))

(defn starting-coords [grid]
  (->> (grid/coords grid)
       (filter #(= \S (grid/at grid %)))
       (first)))

(grid/parse example-grid)
(let [grid (grid/parse example-grid)]
  (grid/neighbors grid (starting-coords grid) grid/cardinal-directions (fn [ch] (not= ch \.))))

(defn neighbors [grid coord]
  (grid/neighbors grid coord grid/cardinal-directions
  (fn [ch] (= ch \#))))

(defn reachable-positions [grid num-steps]
  (loop
   [queue [[(starting-coords grid) num-steps]]
    seen #{}
    result #{}]
    ;; (println queue seen result)
    (if (empty? queue)
      result
      (let [[coord steps-left] (first queue)
            queue (subvec queue 1)
            seen (conj seen [coord steps-left])]
        (if (= steps-left 0)
          (recur queue seen (conj result coord))
          (let [[queue seen]
                (reduce
                 (fn [[queue seen] coord]
                   (if (contains? seen [coord (dec steps-left)])
                     [queue seen]
                     [(conj queue [coord (dec steps-left)])
                      (conj seen [coord (dec steps-left)])]))
                 [queue seen]
                 (neighbors grid coord))]
            (recur queue seen result)))))))

(count (reachable-positions (grid/parse example-grid) 6))
(println (count (reachable-positions (grid/parse-file "2023/day21.txt") 64)))

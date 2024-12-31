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

(grid/bounds (grid/parse example-grid))

(defn neighbors [[xmax ymax] grid [x y]]
  (->>
   (for [[dx dy] grid/cardinal-directions]
    (let [[nx ny] [(+ x dx) (+ y dy)]]
      (case (grid/at grid [(mod nx xmax) (mod ny ymax)])
        \# nil
        [nx ny])))
   (remove nil?)))

(defn reachable-positions [grid num-steps]
  (let [neighbors (partial neighbors (grid/bounds grid))]
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
              (recur queue seen result))))))))

(count (reachable-positions (grid/parse example-grid) 6))
(count (reachable-positions (grid/parse-file "2023/day21.txt") 458))
(+ 327 131)
(let [grid (grid/parse-file "2023/day21.txt")]
  (map
   #(count (reachable-positions grid %))
   '(65 196 327)))


;; solved via quadratic interpolation https://www.johndcook.com/quadratic_interpolator.html
;; 0 -> 3943
;; 1 -> 35126
;; 2 -> 97407
;; answer is at x = (26501365 - 65 ;first square) / 131 ; square length = 202300

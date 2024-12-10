(ns advent2024.day10
  (:require
   [grid :as grid]))

(def example-grid
  '("89010123"
    "78121874"
    "87430965"
    "96549874"
    "45678903"
    "32019012"
    "01329801"
    "10456732"))

(defn num-at [grid coords]
  (let [x (grid/at grid coords)]
    (case x \. Long/MAX_VALUE (Character/getNumericValue x))))

(defn uphill-neighbors [grid curr]
  (let [val-at (num-at grid curr)]
    (->> (grid/neighbors grid curr)
         (filter #(= (- (num-at grid %) val-at) 1)))))

(defn trailheads [grid]
  (->> (grid/coords grid)
       (filter #(= (num-at grid %) 0))))

(defn paths-from [grid trailhead]
  (loop
   [queue [[trailhead]]
    paths #{}]
    (if-let [current-path (first queue)]
      (let [curr (peek current-path)
            queue (rest queue)]
        (if (= (num-at grid curr) 9)
          (recur queue (conj paths current-path))
          (recur
           (reduce
            (fn [queue neighbor]
              (conj queue (conj current-path neighbor)))
            queue
            (uphill-neighbors grid curr))
           paths)))
      paths)))

(paths-from (grid/parse example-grid) [2 0])

(defn reachable-peaks [grid trailhead]
  (->> (paths-from grid trailhead)
       (map last)
       (set)))

(defn score-pt1 [grid trailhead]
  (count (reachable-peaks grid trailhead)))

(trailheads (grid/parse example-grid))
(score-pt1 (grid/parse example-grid) [4 2])

(score-pt1
 (grid/parse example-grid)
 [0 0])

(def grid2 '("10..9.."
             "2...8.."
             "3...7.."
             "4567654"
             "...8..3"
             "...9..2"
             ".....01"))

(let [grid (grid/parse example-grid)]
  (->> (trailheads grid)
       (map (partial score-pt1 grid))
       (reduce +)))

(let [grid (grid/parse-file "2024/day10.txt")]
  (->> (trailheads grid)
       (map (partial score-pt1 grid))
       (reduce +)))

(defn score-pt2 [grid trailhead]
  (count (paths-from grid trailhead)))

(def example-pt2
  '(".....0."
    "..4321."
    "..5..2."
    "..6543."
    "..7..4."
    "..8765."
    "..9...."))

(def example-pt2-1 '("..90..9"
                     "...1.98"
                     "...2..7"
                     "6543456"
                     "765.987"
                     "876...."
                     "987...."))

(def example-pt2-2 '("012345"
                     "123456"
                     "234567"
                     "345678"
                     "4.6789"
                     "56789."))

;; (let [grid (grid/parse-file "2024/day10.txt")]
(let [grid (grid/parse-file "2024/day10.txt")]
  (->> (trailheads grid)
       (map (partial score-pt2 grid))
       (reduce +)))

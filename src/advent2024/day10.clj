(ns advent2024.day10
  (:require
   [grid :as grid]
   [graph :as graph]))

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

(defn reachable-peaks [grid trailhead]
  (let [[reachable & _] (graph/breadth-first-search
                         trailhead
                         (partial uphill-neighbors grid))]
    (->> (keys reachable)
         (filter #(= (num-at grid %) 9)))))

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

(defn downhill-neighbors [grid curr]
  (let [val-at (num-at grid curr)]
    (->> (grid/neighbors grid curr)
         (filter #(= (- (num-at grid %) val-at) -1)))))

;; we can't use graph/all-paths due it not being symmetric
;; I guess we'll just do something similar in our own code
(defn score-pt2 [grid trailhead]
  (let [peaks (reachable-peaks grid trailhead)]
    (->>
     (for [p peaks]
       (loop
        [queue [[p]]
         paths #{}]
         (if-let [current-path (first queue)]
           (let [curr (peek current-path)
                 queue (rest queue)]
             (if (= trailhead curr)
               (recur queue (conj paths current-path))
               (recur
                (reduce
                 (fn [queue neighbor]
                   (conj queue (conj current-path neighbor)))
                 queue
                 (downhill-neighbors grid curr))
                paths)))
           paths)))
     (map count)
     (reduce +))))

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

(let [grid (grid/parse example-grid)]
  (score-pt2 grid [4 0]))

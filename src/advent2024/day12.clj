(ns advent2024.day12
  (:require
   [grid :as grid]
   [clojure.set :as set]
   [utils :as utils]))

;; so this is a combo of flood fill + shoelace (very AoC appropriate)
;; it is more annoying because of "enclaves"
;; can we do this without expansion?
;; without enclaves, we walk the outer border to find the perimeter
;; then we flood fill to find which are totally within?

(def example-plot
  '("AAAA"
    "BBCD"
    "BBCC"
    "EEEC"))

(grid/parse example-plot) set

(defn region-fill [grid]
  (loop [rest-coords (set (grid/coords grid))
         seen #{}
         regions []]
    (if-let [start-coord (first rest-coords)]
      (let [region-letter (grid/at grid start-coord)
            region-seen (loop [queue [start-coord]
                               seen #{}]
                          (if-let [x (first queue)]
                            (recur
                             (reduce
                              (fn [queue n]
                                (if
                                 (contains? seen n)
                                  queue
                                  (conj queue n)))
                              (rest queue)
                              (grid/neighbors grid x grid/cardinal-directions #(not= % region-letter)))
                             (conj seen x))
                            seen))]
        (recur
         (set/difference rest-coords region-seen)
         (set/union seen region-seen)
         (conj regions region-seen)))
      regions)))

(region-fill (grid/parse example-plot))

;; OK so we can expand regions independently to get the perimeter
;; yes that seems easiest.  x,y -> 3x + 1 / 3y + 1

(defn expand-region [grid region-set]
  (reduce
   (fn [acc [x y]]
     (-> acc
         (assoc [(* 3 x) (* 3 y)] \+)
         ;; is there an upper border?
         (assoc [(inc (* 3 x)) (* 3 y)]
                (if (contains? region-set (mapv + [x y] [0 -1]))
                  \space
                  \-))
         (assoc [(+ (* 3 x) 2) (* 3 y)] \+)
         (assoc [(* 3 x) (inc (* 3 y))]
                (if (contains? region-set (mapv + [x y] [-1 0]))
                  \space
                  \|))
         (assoc [(inc (* 3 x)) (inc (* 3 y))] (grid/at grid [x y]))
         (assoc [(+ (* 3 x) 2) (inc (* 3 y))]
                (if (contains? region-set (mapv + [x y] [1 0]))
                  \space
                  \|))
         (assoc [(* 3 x) (+ (* 3 y) 2)] \+)
         (assoc [(inc (* 3 x)) (+ (* 3 y) 2)]
                (if (contains? region-set (mapv + [x y] [0 1]))
                  \space
                  \-))
         (assoc [(+ (* 3 x) 2) (+ (* 3 y) 2)] \+)))
   {}
   region-set))

(defn perimeter [grid region-set]
  (->> (expand-region grid region-set)
       (vals)
       (filter #(or (= % \-) (= % \|)))
       (count)))


(let [grid (grid/parse example-plot)]
  (->> (region-fill grid)
       (map (partial expand-region grid))))


(let [grid (grid/parse example-plot)]
  (->> (region-fill grid)
       (map (fn [region]
              (* (count region) (perimeter grid region))))
       (reduce +)))

(->> example-plot
     (grid/parse))

(def larger-region
  '("RRRRIICCFF"
    "RRRRIICCCF"
    "VVRRRCCFFF"
    "VVRCCCJFFF"
    "VVVVCJJCFE"
    "VVIVCCJJEE"
    "VVIIICJJEE"
    "MIIIIIJJEE"
    "MIIISIJEEE"
    "MMMISSJEEE"))

(let [grid (grid/parse larger-region)]
  (->> (region-fill grid)
       (map (fn [region]
              (* (count region) (perimeter grid region))))
       (reduce +)))

(defn answer-part1 [lines]
  (let [grid (grid/parse lines)]
    (->> (region-fill grid)
         (map (fn [region]
                (* (count region) (perimeter grid region))))
         (reduce +))))

(assert (= (answer-part1 larger-region) 1930))
(assert (= (answer-part1 example-plot) 140))
(answer-part1 (utils/read-input "2024/day12.txt"))

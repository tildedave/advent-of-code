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

(def xo-plot
  '("OOOOO"
    "OXOXO"
    "OOOOO"
    "OXOXO"
    "OOOOO"))

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

(defn is-side? [ch]
  (or (= ch \-) (= ch \|)))

(defn perimeter [grid region-set]
  (->> (expand-region grid region-set)
       (vals)
       (filter is-side?)
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

(defn possible-moves [direction]
  (case direction
    :right [{:next [3 0] :direction :right}
            {:next [1 1] :direction  :down}
            {:next [2 2] :direction  :down}
            {:next [1 -1] :direction :up}
            {:next [2 -2] :direction :up}]
    :up [{:next [0 -3] :direction :up}
         {:next [1 -1] :direction :right}
         {:next [2 -2] :direction :right}
         {:next [-1 -1] :direction :left}
         {:next [-2 -2] :direction :left}]
    :left [{:next [-3 0] :direction :left}
           {:next [-1 -1] :direction  :up}
           {:next [-2 -2] :direction  :up}
           {:next [-1 1] :direction :down}
           {:next [-2 2] :direction :down}]
    :down [{:next [0 3] :direction :down}
           {:next [-1 1] :direction  :left}
           {:next [-2 2] :direction  :left}
           {:next [1 1] :direction :right}
           {:next [2 2] :direction :right}]))

;; expanded region makes the side counting fairly easy I think
;; we'll walk right / up.  only one option should be available
(defn number-of-sides [grid region-set]
  (let [expanded-region (expand-region grid region-set)]
    (loop [side-coords (->> expanded-region
                            (filter (fn [[_ ch]] (is-side? ch)))
                            (map first)
                            (set))
           total-sides 0]
      (if-let [x (first side-coords)]
        (let [[seen new-sides]
              (loop [cl x
                     direction (case (expanded-region x)
                                 \- :right
                                 \| :up)
                     num-sides 0
                     seen #{}]
                (if (contains? seen cl)
                  [seen num-sides]
                  (if-let [next-move (->> (possible-moves direction)
                                          (map (fn [m] (update m :next #(mapv + cl %))))
                                          (filter (fn [m] (contains? side-coords (:next m))))
                                          (first))]
                    (recur
                     (:next next-move)
                     (:direction next-move)
                     (if (= (:direction next-move) direction) num-sides (inc num-sides))
                     (conj seen cl))
                    (assert false "could not find continuation"))))]
          (recur (set/difference side-coords seen) (+ new-sides total-sides)))
        total-sides))))

(let [grid (grid/parse example-plot)]
  (println (number-of-sides grid (first (filter #(contains? % [2 1]) (region-fill grid))))))

(defn answer-part2 [lines]
  (let [grid (grid/parse lines)]
    (->> (region-fill grid)
         (map (fn [region]
                (* (count region) (number-of-sides grid region))))
         (reduce +))))

(def e-map
  '("EEEEE"
    "EXXXX"
    "EEEEE"
    "EXXXX"
    "EEEEE"))

(def tricky-map
  '("AAAAAA"
    "AAABBA"
    "AAABBA"
    "ABBAAA"
    "ABBAAA"
    "AAAAAA"))

(assert (= (answer-part2 example-plot) 80))
(assert (= (answer-part2 larger-region) 1206))
(assert (= (answer-part2 tricky-map) 368))
(assert (= (answer-part2 e-map) 236))
(assert (= (answer-part2 xo-plot) 436))
;; everything right but this one errors
(answer-part2 (utils/read-input "2024/day12.txt"))

(let [grid (grid/parse-file "2024/day12.txt")]
  (number-of-sides grid (first (filter #(contains? % [46 77]) (region-fill grid)))))

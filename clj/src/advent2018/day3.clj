(ns advent2018.day3
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn rectangles-overlap [rect1 rect2]
  ;; x, y is the lower left
  ;; https://stackoverflow.com/questions/306316/determine-if-two-rectangles-overlap-each-other
  ;; assumes that x, y is top left,
  ;; so in their comparisons,
  ;; a.y1 < b.y2 -- y1 here is (+ y1 h), y2 here is y1'
  ;; a.y2 > b.y1 -- y2 here is y1, y1 here is (+ y1' y')
  (let [[x1 y1 w h] rect1
        [x1' y1' w' h'] rect2
        r1 (java.awt.Rectangle. x1 y1 w h)
        r2 (java.awt.Rectangle. x1' y1' w' h')
        ir (.intersection r1 r2)]
    (if (.isEmpty ir) false
        [(.x ir) (.y ir) (.height ir) (.width ir)])))

(rectangles-overlap [1 3 4 4] [3 1 4 4])

(defn squares-within [[x1 y1 w h]]
   (for [dx (range 0 w)
         dy (range 0 h)]
     [(+ x1 dx) (+ y1 dy)]))

(squares-within [1 3 4 4])

(defn parse-claim [s]
  (->> (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$" s)
       (rest)
       (map utils/parse-int)
       ((fn [[a & r]] {a r}))))

(parse-claim "#54 @ 958,978: 15x11")

;; part 1 answer
(->> (utils/read-input "2018/day3.txt")
     (map parse-claim)
     (reduce merge {})
     (vals)
     (map squares-within)
     (apply concat)
     (frequencies)
     (filter (fn [[_ n]] (> n 1)))
     (map first)
     (count))

;; part 2 answer
(let [claims (->> (utils/read-input "2018/day3.txt")
                  (map parse-claim)
                  (reduce merge {}))
      common-coords (->> claims
                         (vals)
                         (map squares-within)
                         (apply concat)
                         (frequencies)
                         (filter (fn [[_ n]] (> n 1)))
                         (map first)
                         (set))]
  (->> claims
       (remove
        (fn [[claim-id rect]]
          (not (empty? (set/intersection common-coords
                                         (set (squares-within rect)))))))
       (first)
       (first)
       ))

;; I guess brute force was great.

(ns advent2017.day14
  (:require [advent2017.day10 :refer [knot-hash]]
            [utils :as utils]
            [grid :as grid]
            [clojure.set :as set]))

;; probably a better way to do this but I'm tired
(defn hex-ch-to-binary [ch]
  (case ch
    \0 '(0 0 0 0)
    \1 '(0 0 0 1)
    \2 '(0 0 1 0)
    \3 '(0 0 1 1)
    \4 '(0 1 0 0)
    \5  '(0 1 0 1)
    \6  '(0 1 1 0)
    \7  '(0 1 1 1)
    \8  '(1 0 0 0)
    \9  '(1 0 0 1)
    \a '(1 0 1 0)
    \b  '(1 0 1 1)
    \c  '(1 1 0 0)
    \d  '(1 1 0 1)
    \e '(1 1 1 0)
    \f '(1 1 1 1)))

(->> (knot-hash "flqrgnkx-0")
     (seq)
     (map hex-ch-to-binary)
     (reduce concat))

(defn grid-coords [input]
  (reduce
   merge
   (for [n (range 0 128)]
     (->> (knot-hash (format "%s-%d" input n))
          (seq)
          (map hex-ch-to-binary)
          (reduce concat)
          (map-indexed (fn [m b] {[m n] b}))
          (reduce merge)))))

(defn answer [input]
  (->> input
       (grid-coords)
       (vals)
       (remove zero?)
       (count)))

;; (answer "flqrgnkx")
;; (answer (first (utils/read-input "2017/day14.txt")))
;; (count (remove zero? (vals (grid-coords "flqrgnkx"))))

;; unfortunately the grid logic requires I pass something like this instead of
;; bounds.
(def fake-grid (cons (repeat 128 0) (repeat 127 0)))

(def neighbors #(grid/neighbors fake-grid % grid/cardinal-directions))

(defn merge-forests [forests coord]
  (let [coord-neighbors (set (neighbors coord))
        [next-forests next]
        (reduce
         (fn [[next-forests curr] forest]
           (if (empty? (set/intersection coord-neighbors forest))
             [(conj next-forests forest) curr]
             [next-forests (set/union forest curr)]))
         [[] #{coord}]
         forests)]
    (conj next-forests next)
    ))

(reduce merge-forests [] [[1 1] [1 2]])

(defn answer-part2 [input]
  (->> (grid-coords input)
       (filter (fn [[coords x]] (= x 1)))
       (map first)
       (reduce merge-forests [])
       (count)))

(time (println (answer-part2 "flqrgnkx")))
(time (println (answer-part2 (first (utils/read-input "2017/day14.txt")))))

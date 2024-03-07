(ns advent2022.day9
  (:require [advent2022.utils :as utils]))


(def lines (utils/read-resource-lines "input/day9.txt"))

(defn parse-line [str]
  (let [[d-str num-str] (.split str " ")]
    (repeat
     (utils/parse-int num-str)
     (case d-str
      "D" :down
      "R" :right
      "L" :left
      "U" :up)
     )))

;; head is at 0, 0
;; tail is at 0, 0

;; we process a step (:up :down :left :right) on the
;; head and the tail position, returning a new head position
;; and a new tail position.
;; at the end the unique # of tail positions is our answer.

(defn touching? [[hx hy] [tx ty]]
  (and (<= (abs (- hx tx)) 1)
       (<= (abs (- hy ty)) 1)))

(defn tail-step [[hx hy] [tx ty]]
  ;; I guess this is really easy.
  (let [dtx (compare hx tx)
        dty (compare hy ty)]
    [(+ dtx tx) (+ dty ty)]))

(defn process-head [[hx hy] direction]
  (case direction
    :up [hx (dec hy)]
    :down [hx (inc hy)]
    :left [(dec hx) hy]
    :right [(inc hx) hy]))

(defn process-tail [[hx hy] [tx ty]]
  (cond
    (touching? [hx hy] [tx ty]) [tx ty]
    :else (tail-step [hx hy] [tx ty])))

(defn process-step [[hx hy] [tx ty] direction]
  (let [new-head (process-head [hx hy] direction)
        new-tail (process-tail [hx hy] [tx ty])]
    [new-head new-tail]))

(def parsed-lines (mapcat parse-line lines))

(def journey (reduce
              (fn [[hpos tpos tails] direction]
                (let [[nh nt] (process-step hpos tpos direction)]
                  [nh nt (cons nt tails)]))
              [[0 0] [0 0] []] parsed-lines))

;; part 1 answer
(count (distinct (nth journey 2)))

;; I suppose part 2 is the same as part 1 with some extra for-loop shenanigans.

(def initial-pos (repeat 10 [0 0]))

(defn single-step [knot-pos-list direction]
  (reduce
   (fn [new-pos-list curr]
     (conj new-pos-list (process-tail (last new-pos-list) curr)))
   [(process-head (first knot-pos-list) direction)]
   (rest knot-pos-list)))

(def journey-p2
  (reduce
   (fn [[knot-pos-list recorded-tails] direction]
     (let [new-positions (single-step knot-pos-list direction)]
       [new-positions (conj recorded-tails (last new-positions))]))
   [initial-pos []] parsed-lines))

(count (distinct (nth journey-p2 1)))

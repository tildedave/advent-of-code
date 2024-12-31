(ns advent2021.day17
  (:require [utils :as utils]
            [clojure.math :as math]))

(def target-re #"^target area: x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)$")

(defn parse-area [str]
  (->> str
       (re-matches target-re)
       (rest)
       (map utils/parse-int)
       (partition 2)
       (map sort)
       (map #(apply vector %))))

;; so no matter what we return to 0 y-wise.
;; we want to return to 0 with velocity = ymin.
;; (as this have the most velocity.)
;; if we return to 0 with velocity ymin the highest we got
;; was 0 + 1 + 2 + ... ymin = ymin(ymin + 1) / 2
(defn answer-part1 [[_ [ymin _]]]
  (quot (* ymin (inc ymin)) 2))

(def example-area (parse-area "target area: x=20..30, y=-10..-5"))
(def input-area (->> (utils/read-input "day17.txt")
                     (first)
                     (parse-area)))

(answer-part1 example-area)
(answer-part1 input-area)

(defn drag [x]
  (cond
    (> x 0) (dec x)
    (< x 0) (inc x)
    :else x))

(defn step [[xpos ypos xvel yvel]]
  [(+ xpos xvel) (+ ypos yvel)
   (drag xvel)
   (dec yvel)])

(defn inside? [[[xstart xend] [ystart yend]] [xpos ypos]]
  (and (<= xstart xpos xend)
       (<= ystart ypos yend)))

(defn done? [area [xpos ypos xvel yvel]]
  (let [[[xstart xend] [ystart yend]] area]
  ;; let's just check if the ypos is below the area,
  ;; since eventually everything will.
    (if (< ypos ystart)
      (cond (< xpos xstart) :left
            (> xpos xend) :right
            :else :below)
      (if (inside? area [xpos ypos])
        :inside
        false))))

(defn is-terminal? [x]
  (not= x false))

(defn target-result [area xvel yvel]
  (->> [0 0 xvel yvel]
       (iterate step)
       (map (partial done? area))
       (filter is-terminal?)
       (first)))

;; for part 2, we still don't need to care about the drag.
;; we DO need to care about negative y-velocities now.

;; x-validity range
(defn valid-x-range [area y]
  (->> (range 1000)
       (mapcat #(list % (- %)))
       (map #(vector % (= (target-result area % y) :inside)))
       (sort-by first)
       (partition-by #(= (second %) true))
       (filter #(= true (second (first %))))
       (first)
       (map first)))

(defn valid-y-range [area xvel]
  (->> (range 1000)
       (mapcat #(list % (- %)))
       (map #(vector % (= (target-result area xvel %) :inside)))
       (sort-by first)
       (partition-by #(= (second %) true))
       (filter #(= true (second (first %))))
       (first)
       (map first)))


(valid-x-range input-area 0)

(defn answer-part2 [area]
  (let [[[xmin xmax] [ymin ymax]] area]
  ;; for positive ys, we have the range we care about based on
  ;; negative ymin/ymax (roughly).
  ;; for negative ys, sort of not clear.  we can just brute force it.
    (->> (for [y (range ymin (- ymin))]
           (map #(vector % y) (valid-x-range area y)))
         (apply concat)
         (set)
         (count))))

;; extremely brute force :-)
(answer-part2 example-area)
(println "part 2 answer" (answer-part2 input-area))

(ns advent2023.day24
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.core.matrix :as m]
            [clojure.set :as set]))

(m/set-current-implementation :vectorz)

(def example-lines
  '("19, 13, 30 @ -2,  1, -2"
    "18, 19, 22 @ -1, -1, -2"
    "20, 25, 34 @ -2, -2, -4"
    "12, 31, 28 @ -1, -2, -1"
    "20, 19, 15 @ 1, -5, -3"))

(defn parse-line [line]
  (->> line
       (re-matches #"^(-?\d+),\s*(-?\d+),\s*(-?\d+)\s*@\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)$")
       (rest)
       (mapv parse-long)
       ((fn [[px py pz vx vy vz]] [[px py pz] [vx vy vz]]))))

(map parse-line example-lines)

;; part 1 asks about collisions within a certain range but whatever
;; OK we only care about the paths colliding so we can have different times.
;; x = px1 + vx1 * t1
;; x = px2 + vx2 * t2
;; y = py1 + vy1 * t1
;; y = py2 + vy2 * t2
;; this is a matrix system - 2 equations in 2 unknowns
;; vx1 * t1 - vx2 * t2 = px2 - px1
;; vy1 * t1 - vy2 * t2 = py2 - py1
;; [vx1 -vx2 | (px2 - px1)]
;; [vy1 -vy2 | (py2 - py1)]
;; we can just solve this via the matrix inversion I guess
;; determinant is 1/(-vx1 * vy2 + vy1 * vx2)

;; skips z axis, also whatever
;; px1 + vx1 * t = px2 + vx2 * t
;; py1 + vy1 * t = py2 + vy2 * t
;; is t an integer? <- no, clear examples otherwise
;; (px1 - px2) = (vx2 - vx1) * t
;; t = (px1 - px2) / (vx2 - vx1)
;; and similarly
;; t = (py1 - py2) / (vy2 - vy1)
;; so this is the collision time
;; then we check if, at that collision time, we're in the desired range



;;;   (let [det (- (* x1 y2) (* x2 y1))
        ;; x-minor (- (* x y2) (* x2 y))
        ;; y-minor (- (* x1 y) (* x y1))
        ;; [t1 t2] [(/ x-minor det) (/ y-minor det)]]


(defn collision-spot [[[px1 py1 _] [vx1 vy1 _]] [[px2 py2 _] [vx2 vy2 _]]]
  ;; [vx1 -vx2]  [t1] = [ px2 - px1 ]
  ;; [vy1 -vy2]  [t2] = [ py2 - py1 ]
  (if-let [[t1 t2] (utils/solve-2d-system [[vx1 (- vx2) (- px2 px1)]
                                           [vy1 (- vy2) (- py2 py1)]])]
    (if (and (> t1 0) (> t2 0))
      (mapv + [px1 py1] [(* t1 vx1) (* t1 vy1)])
      nil)
    nil))


    ;; (if (zero? det)
    ;;   nil
    ;;   (let [x-minor  (- (* (- px2 px1)  )) (- (* x y2) (* x2 y))
    ;;         [t1 t2] (/ x-minor det (/ y-minor det))
    ;;     ;; OK this is the correct inverse matrix, tx ty are just multiplying
    ;;         [[t1] [t2]] (m/mmul inv-matrix a)]
    ;;     (if (and (> t1 0) (> t2 0))
    ;;       (mapv float (mapv + [px1 py1] [(* t1 vx1) (* t1 vy1)]))
    ;;       nil)))))

(defn collision-in-bounds? [[[xmin xmax] [ymin ymax]] particle1 particle2]
  (let [spot (collision-spot particle1 particle2)]
    (if (nil? spot)
      false
      (let [[x y] spot]
        (and (<= xmin x xmax) (<= ymin y ymax))))))

;; example answer
(as-> example-lines v
  (map parse-line v)
  (combo/combinations v 2)
  (filter #(apply (partial collision-in-bounds? [[7 27] [7 27]]) %) v)
  (count v))

(collision-spot
 (parse-line "19, 13, 30 @ -2, 1, -2")
 (parse-line "18, 19, 22 @ -1, -1, -2"))

(collision-spot
 (parse-line "18, 19, 22 @ -1, -1, -2")
 (parse-line "20, 25, 34 @ -2, -2, -4"))

(as-> (utils/read-input "2023/day24.txt") v
  (map parse-line v)
  (combo/combinations v 2)
  (filter #(apply (partial collision-in-bounds? [[200000000000000 400000000000000] [200000000000000 400000000000000]]) %) v)
  (count v))

;; try this idea, which relies on finding x/y/z from similar hailstone velocities
;; https://old.reddit.com/r/adventofcode/comments/18pnycy/2023_day_24_solutions/keqf8uq/
;; take all velocities -1000 -> 1000
;; group hailstones by their x-coord

(as-> (utils/read-input "2023/day24.txt") v
  (map parse-line v))

(defn possible-vals [hailstones selector]
  (reduce set/intersection
          (for [h1 hailstones
                h2 hailstones
                :when (and
                       (not= h1 h2)
                       (= (selector (second h1)) (selector (second h2)))
                       (> (selector (second h1)) 100))
                :let [pos-diff (- (selector (first h2)) (selector (first h1)))
                      velocity (selector (second h1))
               ;; 1000 is an arbitrary cutoff
                      all-divisors (utils/all-divisors pos-diff 1000)]]
            (->> all-divisors
                 (mapcat (fn [d] [(+ d velocity) (+ (- d) velocity)]))
                 (set)))))

;; no degeneracy in example :shrug:
;; (possible-vals  #(nth % 2))

(defn answer-part2 []
  (let [hailstones (map parse-line (utils/read-input "2023/day24.txt"))
        velocity (for [i (range 0 3)
                       :let [comp (first (possible-vals hailstones #(nth % i)))]]
                   comp)
        [vx vy vz] velocity
        [h1 h2] hailstones
        stone1 [(first h1) (mapv - (second h1) velocity)]
        stone2 [(first h2) (mapv - (second h2) velocity)]
        [px py] (collision-spot stone1 stone2)
        [px1 py1 pz1] (first h1)
        [vx1 vy1 vz1] (second h1)
        t1 (/ (- px px1) (- vx1 vx))
        t1' (/ (- py py1) (- vy1 vy))
        ;; pz + (vz * t1) = pz1 + (vz1 * t1)
        pz (+ pz1 (- (* t1 vz)) (* t1 vz1))]
    (+ px py pz)))

(assert (= (answer-part2) 578177720733043))

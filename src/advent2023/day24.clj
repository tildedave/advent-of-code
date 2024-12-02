(ns advent2023.day24
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]))

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

(defn matrix-mult [[[x11 x12] [x21 x22]] [[y11 y12] [y21 y22]]]
  [[(+ (* x11 y11) (* x12 y21)) (+ (* x11 y12) (* x12 y22))]
   [(+ (* x21 y11) (* x22 y21)) (+ (* x21 y12) (* x22 y22))]])

(defn matrix-vector-mult [[[x11 x12] [x21 x22]] [[y] [z]]]
  [[(+ (* x11 y) (* x12 z))]
   [(+ (* x21 y) (* x22 z))]])

(defn collision-spot [[[px1 py1 _] [vx1 vy1 _]] [[px2 py2 _] [vx2 vy2 _]]]
  (let [det (+ (* (- vx1) vy2) (* vy1 vx2))]
    (if (= det 0)
      nil
      (let [m [[vx1 (- vx2)] [vy1 (- vy2)]]
            inv-matrix [[(* (/ 1 det) (- vy2)) (* (/ 1 det) vx2)]
                        [(* (/ 1 det) (- vy1)) (* (/ 1 det) vx1)]]
        ;; OK this is the correct inverse matrix, tx ty are just multiplying
            [[t1] [t2]] (matrix-vector-mult inv-matrix [[(- px2 px1)] [(- py2 py1)]])]
        (if (and (> t1 0) (> t2 0))
          (mapv float (mapv + [px1 py1] [(* t1 vx1) (* t1 vy1)]))
          nil)))))

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

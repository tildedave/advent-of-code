(ns advent2017.day15
  (:require [utils :as utils]))

;; (bit-shift-left 1 16)
;; (mod 245556042 65536)
;; (mod 1431495498 65536)

(mod 1092455 65536)
(mod 430625591 65536)
(mod 1181022009 65536)
(mod 1233683848 65536)

;; we are considering everything mod 65536
;; the multiplicative group has the order
;; 3 × 5 × 17 × 257
;; so A and B generate subgroups of some order
;; when they sync up for the first time we can
;; calculate the rest of the intersection points.
;; it seems likely that A and B are chosen so that
;; they aren't nilpotent.
;; let's discover the order of A and B.

(defn a-step [prev]
  (mod (* prev 16807) 2147483647))

(defn b-step [prev]
  (mod (* prev 48271) 2147483647))

(iterate a-step 65)

(filter #(= (second %) 58186)
        (map-indexed vector (map #(mod % 65536) (iterate a-step 65))))
(mod 245556042 65536)

(- 549194 3)

(def last-16-bits #(mod % 65536))

;; part 1
(->>
 (map
  (partial =)
  (map last-16-bits (take 40000000 (iterate a-step 883)))
  (map last-16-bits (take 40000000 (iterate b-step 879))))
 (remove false?)
 (count))


(->>
 (map
  (partial =)
  (map last-16-bits
       (take 5000000 (filter #(zero? (mod % 4)) (iterate a-step 883))))
  (map last-16-bits
       (take 5000000 (filter #(zero? (mod % 8)) (iterate b-step 879)))))
 (remove false?)
 (count))

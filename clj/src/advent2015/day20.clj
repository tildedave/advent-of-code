(ns advent2015.day20
  (:require [utils :as utils]))

;; https://oeis.org/A000203
;; This is just the sigma function
;; Ireland & Rosen gives the formula
;; if n = p1^a1 \cdot p2^a2 \cdot ...
;; sigma(n) = (p1^(a1+1) - 1)/(p1 - 1) ...

;; the numbers we're dealing with are pretty small,
;; so we can probably just brute force this.

(defn sigma [n]
  (loop [d-list (utils/divisors n)
         result 1]
    (if-let [[p n] (first d-list)]
      (recur
       (rest d-list)
       (* result
          (/ (dec (reduce * (repeat (inc n) p))) ;; naive exponentiation
             (dec p))))
      result)))

(defn answer-part1 [target]
  (->> (range)
       (drop 2)
       (filter #(>= (* (sigma %) 10) target))
       (first)))

;; (answer-part1 29000000)

(defn modded-sigma [n]
  (- (sigma n)
     (let [m (/ (dec n) 50)]
       (loop [q 1
              total 0]
         (cond
           (> q m) total
           (zero? (mod n q)) (recur (inc q) (+ total q))
           :else (recur (inc q) total))))))

(defn answer-part2 [target]
  (->> (range)
       (drop 2)
       (filter #(>= (* (modded-sigma %) 11) target))
       (first)))

(println "I answer" (answer-part2 29000000))

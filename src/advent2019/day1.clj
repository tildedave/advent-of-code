(ns advent2019.day1
  (:require [utils :as utils]))

(defn fuel-requirement [mass]
  (- (quot mass 3) 2))

(fuel-requirement 100756)

;; answer 1
(->> (utils/read-input "2019/day1.txt")
     (map utils/parse-int)
     (map fuel-requirement)
     (reduce +))

(defn total-fuel-requirement [mass]
  (let [f (fuel-requirement mass)]
    (if (<= f 0)
      '()
      (conj (total-fuel-requirement f) f))))

;; answer 2
(->> (utils/read-input "2019/day1.txt")
     (map utils/parse-int)
     (map total-fuel-requirement)
     (map (partial reduce + 0))
     (reduce +))

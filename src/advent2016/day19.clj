(ns advent2016.day19
  (:require [clojure.math :as math]
            [clojure.string :as string]
            [utils :as utils]))

(into [1 2 3] [4])
(defn step [[state order]]
  (if (= (count order) 1)
    [state order]
    (let [[x y] order
          rest-x (subvec order 2)]
      [(-> state
          (update x (partial + (state y)))
          (assoc y 0))
       (into rest-x [x])])))

(defn initial-state [n]
  [(reduce merge (map #(hash-map % 1) (range 1 (inc n)))) (vec (range 1 (inc n)))])

(initial-state 5)

(defn answer [n]
  (reduce
   (fn [acc [_ order]]
     (if (= (count order) 1)
       (reduced (first order))
       acc))
   nil
   (iterate step (initial-state n))))

(map-indexed (fn [n x] [(inc n) x]) (map answer (drop 1 (range))))

(math/log1p 3005290)

(iterate step (initial-state 6))

;; https://oeis.org/search?q=1+1+3+1+3+5+7+1+&language=english&go=Search
;; josephus problem
;; answer comes from taking the binary sequence of a number, shifting right,
;; setting that as MSB.

(defn to-binary-vec [n]
  (let [num (inc (int (/ (math/log n) (math/log 2))))]
    (loop [n n
           idx (dec num)
           v (vec (repeat num 0))]
      (if
        (= n 0) v
        (recur
         (quot n 2)
         (dec idx)
         (assoc v idx (mod n 2)))))))

(defn from-binary-vec [v]
  (Integer/parseInt (string/join v) 2))

(defn actual-answer [n]
  (let [v (to-binary-vec n)]
    (from-binary-vec (into (subvec v 1) [(get v 0)]))))

(actual-answer (utils/parse-int (first (utils/read-input "2016/day19.txt"))))

;; the puzzle input is 3 million.
;; the difference between this and the example is that it's even.

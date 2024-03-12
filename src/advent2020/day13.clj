(ns advent2020.day13
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn parse-input [lines]
  (let [[timestamp buses] (utils/read-input (format "2020/%s" lines))]
    [(utils/parse-int timestamp)
     (->> (string/split buses #",")
          (map utils/try-parse-int))]))

(defn answer-part1 [lines]
  (let [[n moduli] (parse-input lines)]
    (->> moduli
         (remove (partial = "x"))
         (map #(vector % (- % (mod n %))))
         (sort-by second)
         (first)
         (reduce *))))

(answer-part1 "day13-example.txt")
(answer-part1 "day13.txt")

;; so obvious part2 is the CRT.  CRT is life

;; Knuth Algo X 4.5.2
(defn euclid-extended [u v]
  (loop
   [uv [1 0 u] vv [0 1 v]]
    (let [[_ _ u3] uv
          [_ _ v3] vv]
      (if (zero? v3) uv
        (let [q (quot u3 v3)]
          (recur
           vv
           (mapv - uv (mapv (partial * q) vv))))))))

;; 1.3.12 Course in Computational Algebraic Number Theory
(defn crt-inductive [residues]
  (loop [[x m] (first residues)
         residues (rest residues)]
    (if (empty? residues) x
        (let [[x' m'] (first residues)
              [u v d] (euclid-extended m m')
              ;; *' "auto-promotes" to bigint
              [x m] [(+ (*' u m x') (*' v m' x)) (* m m')]]
          (assert (= d 1))
          (recur [(mod x m) m] (rest residues))))))

(assert (= (crt-inductive '([0 3] [3 4] [4 5])) 39))

(defn answer-part2 [lines]
  (let [[_ moduli] (parse-input lines)]
    (->> moduli
         (map-indexed vector)
         (remove #(= "x" (second %)))
         (map (fn [[n x]] [(- x n) x]))
         (crt-inductive)
         )))

(answer-part2 "day13-example.txt")
(answer-part2 "day13.txt")

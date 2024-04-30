(ns advent2016.day19
  (:require [clojure.math :as math]
            [clojure.string :as string]
            [utils :as utils]
            [clojure.data.int-map :as i]))

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

;; part 2 looks kind of annoying.
;; maybe it will be fine.
;; idea - basically implement the thing, confirm it's correct, try to some
;; kind of sequence thing to see its behavior.

(initial-state 5)

(defn answer [state-seq]
  (reduce
   (fn [acc [_ order]]
     (if (= (count order) 1)
       (reduced (first order))
       acc))
   nil
   state-seq))

(map-indexed (fn [n x] [(inc n) x]) (map #(answer (iterate step (initial-state %))) (drop 1 (range))))

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

(defn answer-part1 [n]
  (let [v (to-binary-vec n)]
    (from-binary-vec (into (subvec v 1) [(get v 0)]))))

(answer-part1 (utils/parse-int (first (utils/read-input "2016/day19.txt"))))

;; the puzzle input is 3 million.
;; the difference between this and the example is that it's even.

(defn step-part2 [[state order]]
;;   (println (count order) "remain")
  (if (= (count order) 1)
    [state order]
    (let [[x] order
          y-idx (quot (count order) 2)
          y (get order y-idx)]
      [(-> state
           (update x (partial + (state y)))
           (assoc y 0))
       (-> (subvec order 1 y-idx)
           (into (into (subvec order (inc y-idx)) [x])))])))

(map-indexed (fn [n x] [(ic n) x]) (map #(answer (iterate step-part2 (initial-state %))) (drop 1 (range))))


;; https://oeis.org/search?q=1+1+3+1+2+3+5+7+9&language=english&go=Search
;; cowboy shootout problem

(defn highest-power-of-3 [n]
  (int (math/pow 3
            (dec (loop [option 0]
    (if (<= (math/pow 3 option) n)
      (recur (inc option))
      option))))))

(defn answer-part2 [n]
  (let [x (highest-power-of-3 n)]
    (cond (= x n) x
          (< n (* 2 x))
          (mod n x)
          :else (+ x (* 2 (mod n x))))))

(answer-part2 100)
(answer-part2 (utils/parse-int (first (utils/read-input "2016/day19.txt"))))


(highest-power-of-3 5)

(println (answer (iterate step-part2 (initial-state 3005290))))

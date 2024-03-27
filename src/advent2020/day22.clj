(ns advent2020.day22
  (:require [utils :as utils]))

(utils/read-input "2020/day22.txt")
(defn parse-input [filename]
  (->> filename
       (format "2020/%s" filename)
       (utils/read-input)
       (utils/split-by "")
       (map rest)
       (map #(mapv utils/parse-int %))
       (map-indexed vector)
       (into {})))

(defn step [deck]
  (let [[deck1 deck2] [(get deck 0) (get deck 1)]
        [c1] deck1
        [c2] deck2]
    (cond
      (> c1 c2) {0 (-> (subvec deck1 1)
                       (conj c1)
                       (conj c2))
                 1 (subvec deck2 1)}
      (> c2 c1) {0 (subvec deck1 1)
                 1 (-> (subvec deck2 1)
                       (conj c2)
                       (conj c1))})))

(defn final-score [deck]
  (->> deck
       (reverse)
       (map-indexed vector)
       (map (fn [[n x]] (* (inc n) x)))
       (reduce +)))

(defn answer-part1 [filename]
  (reduce
   (fn [_ state]
     (cond
       (empty? (get state 0)) (reduced (final-score (get state 1)))
       (empty? (get state 1)) (reduced (final-score (get state 0)))
       :else nil))
   (iterate step (parse-input filename))))

(answer-part1 "day22-example.txt")
(answer-part1 "day22.txt")

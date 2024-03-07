(ns advent2021.day25
  (:require [advent2021.grid :as grid]
            [advent2021.utils :as utils]
            [clojure.set :as set]))

(defn cucumber-positions [grid ch]
  (->> (grid/coords grid)
       (filter (fn [[x y]] (= ch (get-in grid [y x]))))
       (set)))

(defn cucumber-step [grid [x y]]
  (let [cucumber (get-in grid [y x])
        next (case cucumber
               \> [(inc x) y]
               \v [x (inc y)])]
    (if (grid/out-of-bounds? grid next)
      (case cucumber
        \> [0 y]
        \v [x 0])
      next)))

(defn grid-step [grid herd-positions other-herd-positions]
  (let [changed-positions
        (->> herd-positions
             (map #(vector % (cucumber-step grid %)))
             (remove #(or (contains? herd-positions (second %))
                          (contains? other-herd-positions (second %)))))]
    [(reduce
     (fn [grid [[ox oy] [nx ny]]]
       (let [cucumber (get-in grid [oy ox])]
       (-> grid
           (assoc-in [oy ox] \.)
           (assoc-in [ny nx] cucumber))))
     grid
     changed-positions)
     (-> herd-positions
         (set/difference (->> changed-positions (map first) (set)))
         (set/union (->> changed-positions (map second) (set))))]
     ))

(defn grid-seq [filename]
  (let [grid (grid/parse (utils/read-input filename))]
    (->>
     (iterate
      (fn [[grid east-positions south-positions]]
        (let [[grid next-east-positions] (grid-step grid east-positions south-positions)
              [grid next-south-positions] (grid-step grid south-positions next-east-positions)]
          [grid next-east-positions next-south-positions]))
      [grid (cucumber-positions grid \>) (cucumber-positions grid \v)]))))

(defn answer-part1 [filename]
  (let [indexed-seq (->> (grid-seq filename)
                         (map-indexed vector))]
    (->> (map (fn [[n [_ east-pos south-pos]] [_ [_ east-pos' south-pos']]]
                [n (and (= east-pos east-pos')
                     (= south-pos south-pos'))])
                indexed-seq (rest indexed-seq))
         (filter #(true? (second %)))
         (first)
         (first)
         (inc))))

;; the end!
(answer-part1 "day25.txt")

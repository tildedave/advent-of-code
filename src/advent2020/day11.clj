(ns advent2020.day11
  (:require [grid :as grid]
            [utils :as utils]))

(defn seats [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \L))
       (set)))

(defn step [grid seat-coords]
  (fn [[occupied _]]
    (let [changes
          (remove
           nil?
           (for [seat seat-coords]
             (let [is-occupied (contains? occupied seat)
                   occupied-neighbors (->> (grid/neighbors-with-diagonals grid seat)
                                           (filter (partial contains? occupied)))]
               (cond
                 (and (not is-occupied) (empty? occupied-neighbors)) [:occupy seat]
                 (and is-occupied (>= (count occupied-neighbors) 4)) [:empty seat]
                 :else nil))))]
    [(reduce
     (fn [occupied [action coord]]
       (case action
         :occupy (conj occupied coord)
         :empty (disj occupied coord)))
     occupied
     changes)
     changes]
    )))

(defn answer-part1 [filename]
  (let [grid (grid/parse (utils/read-input (format "2020/%s" filename)))
        seat-seq (iterate (step grid (seats grid)) [#{} nil])]
    (reduce
     (fn [acc [occupied changes]]
       (cond
         (nil? changes) acc
         (empty? changes) (reduced (count occupied))
         :else acc))
     -1
     seat-seq)))

(answer-part1 "day11-example.txt")
(answer-part1 "day11.txt")

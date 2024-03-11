(ns advent2020.day11
  (:require [grid :as grid]
            [utils :as utils]))

(defn seats [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \L))
       (set)))

(defn get-neighbors [grid]
  (memoize (fn [seat]
             (grid/neighbors grid seat grid/all-directions))))

(defn step [grid seat-coords]
  (let [get-neighbors (get-neighbors grid)]
  (fn [[occupied _]]
    (let [changes
          (remove
           nil?
           (for [seat seat-coords]
             (let [is-occupied (contains? occupied seat)
                   occupied-neighbors (->> (get-neighbors seat)
                                           (filter (partial contains? occupied)))]
               (cond
                 (and (not is-occupied) (empty? occupied-neighbors)) [:occupy seat]
                 (and is-occupied (>= (count occupied-neighbors) 4)) [:empty seat]
                 :else nil))))]
    [(loop
      [occupied occupied
       [[action coord] & changes] changes]
       (if (empty? changes) occupied
           (recur
            (case action
             :occupy (conj occupied coord)
             :empty (disj occupied coord))
            changes)))
     changes]
    ))))

(defn answer-part1 [filename]
  (let [grid (grid/parse (utils/read-input (format "2020/%s" filename)))
        step-fn (step grid (seats grid))]
    (loop [n 0 state [#{} '(:first)]]
      (if
       (empty? (second state))
        (count (first state))
        (recur (inc n) (step-fn state))))))

(answer-part1 "day11-example.txt")
(answer-part1 "day11.txt")

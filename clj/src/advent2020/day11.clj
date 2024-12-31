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

(defn apply-changes [occupied changes]
  (if (empty? changes) occupied
      (loop
       [occupied occupied
        [[action coord] & changes] changes]
        (let [next-occupied (case action
                              :occupy (conj occupied coord)
                              :empty (disj occupied coord))]
          (if (empty? changes) next-occupied
              (recur next-occupied changes))))))

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
    [(apply-changes occupied changes) changes]
    ))))

(utils/read-input "2020/day11-example.txt")

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

(defn get-neighbors-part2 [grid]
  (memoize
   (fn [seat]
     (->> (for [direction grid/all-directions]
            (loop [curr (grid/add seat direction)]
              (condp = (grid/at grid curr)
                nil nil
                \L curr
                \. (recur (grid/add curr direction)))))
          (remove nil?)))))

(defn step-part2 [grid seat-coords]
  (let [get-neighbors (get-neighbors-part2 grid)]
    (fn [[occupied _]]
      (let [changes (->>
                     (for [seat seat-coords]
                   ;; OK thank goodness this is not looking for occupied seats,
                   ;; just ANY seats.
                       (let [occupied-neighbors (->> (get-neighbors seat)
                                                     (filter (partial contains? occupied)))]
                         (if (contains? occupied seat)
                           (if (>= (count occupied-neighbors) 5)
                             [:empty seat]
                             nil)
                           (if (empty? occupied-neighbors)
                             [:occupy seat]
                             nil))))
                     (remove nil?))]
        [(apply-changes occupied changes) changes]))))

(defn answer-part2 [filename]
  (let [grid (grid/parse (utils/read-input (format "2020/%s" filename)))
        step-fn (step-part2 grid (seats grid))
        seat-seq (iterate step-fn [#{} '(:first)])]
    (loop [[[occupied changes] & seat-seq] seat-seq]
      (if (empty? changes) (count occupied)
          (recur seat-seq)))))

(time (answer-part2 "day11-example.txt"))
(time (answer-part2 "day11.txt"))

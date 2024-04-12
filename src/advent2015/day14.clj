(ns advent2015.day14
  (:require [utils :as utils]))

(def reindeer-re #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.")

(defn parse-reindeer [line]
  (let [[_ name speed move-time rest-time] (->> line
                                                (re-matches reindeer-re)
                                                (map utils/try-parse-int))]
    {name {:speed speed :move-time move-time :rest-time rest-time}}))

(defn reindeer-step [reindeers status reindeer]
  (let [{:keys [speed move-time rest-time]} (reindeers reindeer)
        {:keys [rest-left move-left]} (status reindeer)]
    (cond
      (> rest-left 0) (update-in status [reindeer :rest-left] dec)
      (> move-left 0) (-> status
                          (update-in [reindeer :move-left] dec)
                          (update-in [reindeer :distance] (fnil (partial + speed) 0)))
      (= move-left 0) (-> status
                          (assoc-in [reindeer :rest-left] (dec rest-time))
                          (assoc-in [reindeer :move-left] -1))
      (= rest-left 0) (-> status
                          (update-in [reindeer :distance] (fnil (partial + speed) 0))
                          (assoc-in [reindeer :move-left] (dec move-time)))

      )))

(defn step [reindeers status]
  ;; update each reindeer's status.
  (reduce (partial reindeer-step reindeers)
          status
          (keys reindeers)))

(defn initial-state [reindeers]
  (->> (for [reindeer (keys reindeers)] {reindeer {:move-left -1 :rest-left 0}})
       (reduce merge)))

(defn flying-seq [filename]
  (let [reindeers (->> (utils/read-input (format "2015/%s" filename))
                       (map parse-reindeer)
                       (reduce merge))]
    (iterate
     (partial step reindeers)
     (initial-state reindeers))))

(defn answer [filename seconds]
    (->>
     (nth (flying-seq filename) seconds)
     (vals)
     (map :distance)
     (sort)
     (last)))

(nth (flying-seq "day14-example.txt") 138)

(answer "day14-example.txt" 1000)
(answer "day14.txt" 2503)

(sort-by first > [[4 5] [1 2] [2 3]])

(defn answer-part2 [filename seconds]
  (->>
   (reduce
   (fn [scores state]
     (let [reindeer-by-score (->> state
                                  (map (fn [[reindeer status]] [reindeer (get status :distance 0)]))
                                  (sort-by second >))
           max-score (second (first reindeer-by-score))
           winning-reindeer (filter #(= (second %) max-score) reindeer-by-score)]
       (reduce
        (fn [scores winner]
          (update scores (first winner) (fnil inc 0)))
        scores
        winning-reindeer)))
   {}
   ;; don't count second 0 as contributing to the score
   (take seconds (rest (flying-seq filename))))
   (vals)
   (sort-by >)
   (first)))

(answer-part2 "day14-example.txt" 1000)
(answer-part2 "day14.txt" 2503)

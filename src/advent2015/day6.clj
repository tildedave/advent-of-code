(ns advent2015.day6
  (:require [utils :as utils]))


;; I suppose this is day 22 2021 all over again, with the positive and
;; negative cuboids.
;; array size of 1 million can be brute forced/done "simply".
;; I guess we will do that.

(defn parse-line [line]
  (let [[_ action x1 y1 x2 y2]
    (re-matches #"^(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)$" line)]
    {:action action
     :coords (map utils/parse-int [x1 y1 x2 y2])}))

(parse-line "turn on 0,0 through 999,999")

(defn interior-coords [[x1 y1 x2 y2]]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [x y]))

;; state -> turned on lights
(defn process-line [state line]
  (let [{:keys [action coords]} (parse-line line)]
    (reduce
     (case action
       "turn on" conj
       "turn off" disj
       "toggle" (fn [state [x y]] (if (contains? state [x y])
                                    (disj state [x y])
                                    (conj state [x y]))))
     state
     (interior-coords coords))))

(defn answer-part1 []
  (->> (utils/read-input "2015/day6.txt")
       (reduce process-line #{})
       (count)))

(time (answer-part1))

(defn process-line-p2 [state line]
  (let [{:keys [action coords]} (parse-line line)]
     (reduce
      (case action
        "turn on" (fn [state coords] (update state coords (fnil inc 0)))
        "turn off" (fn [state coords] (update state coords (fnil (fn [n] (if (= n 0) 0 (dec n))) 0)))
        "toggle" (fn [state coords] (update state coords (fnil #(inc (inc %)) 0))))
      state
      (interior-coords coords))))

(defn answer-part2 []
  (->> (utils/read-input "2015/day6.txt")
       (reduce process-line-p2 {})
       (vals)
       (reduce +)))

(answer-part2)

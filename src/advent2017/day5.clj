(ns advent2017.day5
  (:require [utils :as utils]))

(defn step-part1 [[num-vec n]]
  (if-let [offset (get num-vec n)]
    [(update num-vec n inc)
     (+ n offset)]
    [num-vec n]))

(defn answer [step-fn nums]
  (reduce
   (fn [steps [num-vec n]]
     (if-let [_ (get num-vec n)]
       (inc steps)
       (reduced steps)))
   0
   (iterate step-fn [nums 0])))

(answer step-part1 [0 3 0 1 -3])
(answer step-part1 (->> (utils/read-input "2017/day5.txt") (mapv utils/parse-int)))

(defn step-part2 [[num-vec n]]
  (if-let [offset (get num-vec n)]
     [(update num-vec n #(if (>= % 3) (dec %) (inc %)))
      (+ n offset)]
     [num-vec n]))

(answer step-part2 [0 3 0 1 -3])
(answer step-part2 (->> (utils/read-input "2017/day5.txt") (mapv utils/parse-int)))

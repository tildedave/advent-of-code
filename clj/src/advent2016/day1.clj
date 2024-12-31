(ns advent2016.day1
  (:require [utils :as utils]))

(defn parse-direction [s]
  (->> s
       (re-matches #"(L|R)(\d+)")
       (rest)
       (map utils/try-parse-int)))

(defn turn-right [dir]
  (case dir
    :north :east
    :east :south
    :south :west
    :west :north))

(defn turn-left [dir]
  (-> dir (turn-right) (turn-right) (turn-right)))

(defn stepper-p1 [instrs]
  (loop [instrs instrs
         [x y] [0 0] dir :north]
    (if-let [[turn-direction num-steps] (first instrs)]
      (let [new-dir (case turn-direction
                      "R" (turn-right dir)
                      "L" (turn-left dir))]
        (recur
         (rest instrs)
         (case new-dir
           :north [x (- y num-steps)]
           :south [x (+ y num-steps)]
           :east [(+ x num-steps) y]
           :west [(- x num-steps) y])
         new-dir))
      (+ (abs x) (abs y)))))

(defn process-direction-string [str stepper]
  (->> (.split str ", ")
       (map parse-direction)
       (stepper)))

(process-direction-string "R5, L5, R5, R3" stepper-p1)

(defn answer-part1 []
  (process-direction-string
   (first (utils/read-input "2016/day1.txt"))
   stepper-p1
   ))

(answer-part1)

(defn stepper-p2 [instrs]
  (loop [instrs instrs
         [x y] [0 0]
         dir :north
         steps-left 0
         seen-locations #{}]
    (cond
      (contains? seen-locations [x y]) (+ (abs x) (abs y))
      (> steps-left 0)
      (recur
       instrs
       (case dir
         :north [x (dec y)]
         :south [x (inc y)]
         :east [(inc x) y]
         :west [(dec x) y])
       dir
       (dec steps-left)
       (conj seen-locations [x y]))
      :else
      (if-let [[turn-direction num-steps] (first instrs)]
        (recur
         (rest instrs)
         [x y]
         (case turn-direction
           "R" (turn-right dir)
           "L" (turn-left dir))
         num-steps
         seen-locations)
        (throw (Exception. "reached end of instructions without visiting twice"))))))

(parse-direction "L5")

(process-direction-string "R8, R4, R4, R8" stepper-p2)

(defn answer-part2 []
  (process-direction-string
   (first (utils/read-input "2016/day1.txt"))
   stepper-p2))

(answer-part2)

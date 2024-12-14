(ns advent2024.day14
  (:require
    [utils :as utils]))

(def example-robots
  '("p=0,4 v=3,-3"
    "p=6,3 v=-1,-3"
    "p=10,3 v=-1,2"
    "p=2,0 v=2,-1"
    "p=0,0 v=1,3"
    "p=3,0 v=-2,-2"
    "p=7,6 v=-1,-3"
    "p=3,0 v=-1,-2"
    "p=9,3 v=2,3"
    "p=7,3 v=-1,2"
    "p=2,4 v=2,-3"
    "p=9,5 v=-3,-3"))


(defn robot-move [xmax ymax [px py vx vy]]
  [(mod (+ px vx) xmax)
   (mod (+ py vy) ymax)
   vx
   vy])

(defn quadrant [xmax ymax [px py _ _]]
  (let [[mx my] [(/ (dec xmax) 2) (/ (dec ymax) 2)]]
    (cond (= px mx) -1
          (= py my) -1
          :else
          (+ (if (< px mx) 0 1)
             (if (<= py my) 0 2)))))

(quadrant 11 7 [0 2])

(as-> example-robots v
  (map #(vec (utils/str->nums %)) v)
  (iterate #(map (partial robot-move 11 7) %) v)
  (drop 100 v)
  (first v)
  (group-by (partial quadrant 11 7) v)
  (dissoc v -1)
  (vals v)
  (map count v)
  (reduce * v))

(as-> (utils/read-input "2024/day14.txt") v
  (map #(vec (utils/str->nums %)) v)
  (iterate #(map (partial robot-move 101 103) %) v)
  (drop 100 v)
  (first v)
  (group-by (partial quadrant 101 103) v)
  (dissoc v -1)
  (vals v)
  (map count v)
  (reduce * v))

(ns advent2024.day14
  (:require
   [utils :as utils]
   [clojure.string :as string]))

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

(defn parse-robots [lines]
  (map #(vec (utils/str->nums %)) lines))

(as-> (parse-robots (utils/read-input "2024/day14.txt")) v
  (iterate #(map (partial robot-move 101 103) %) v)
  (drop 100 v)
  (first v)
  (group-by (partial quadrant 101 103) v)
  (dissoc v -1)
  (vals v)
  (map count v)
  (reduce * v))

(defn robot-positions [robots]
  (->> robots (map (partial take 2)) (frequencies)))

;; we'll assume in the christmas tree every robot is next to every other one
(defn adjacent-robot-count [robots]
  ;; we can just do this dumbly, no need to handle wraps
  (let [robot-positions (robot-positions robots)]
    (->> robots
         (remove (fn [[x y]]
                    (->>
                     (for [[dx dy] [[-1 0] [1 0] [0 1] [0 -1]]]
                       [dx dy])
                     (map #(mapv + % [x y]))
                     (filter #(contains? robot-positions %))
                     (first)
                     (nil?))))
         (count))))

(adjacent-robot-count (parse-robots example-robots))


(time (as-> (parse-robots (utils/read-input "2024/day14.txt")) v
  (iterate #(map (partial robot-move 101 103) %) v)
  (map-indexed vector v)
  (map (fn [[n r]] [n (adjacent-robot-count r)]) v)
  (drop 1000 v)
        (first v)))

(defn robots-to-string [xmax ymax robots]
  (let [robot-positions (robot-positions robots)]
    (string/join "\n"
    (for [y (range 0 ymax)]
      (string/join
       (for [x (range 0 xmax)]
        (if (contains? robot-positions [x y])
          (robot-positions [x y])
          \.)))))))

(println (robots-to-string 11 7 (parse-robots example-robots)))


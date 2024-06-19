(ns advent2019.day12
  (:require [utils :as utils]))

(defn parse-moon [line]
  (->> line
       (re-matches #"^<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>$")
       (rest)
       (map utils/parse-int)
       ((fn [[x y z]] {:x x :y y :z z :vx 0 :vy 0 :vz 0}))))


(def axis-velocity {:x :vx :y :vy :z :vz})

(defn apply-gravity [moons]
  (for [moon1 moons]
    (reduce
     (fn [moon [k v]]
       (update moon k (partial + v)))
     moon1
     (for [axis [:x :y :z]]
       [(axis-velocity axis)
        (reduce + (for [moon2 moons]
                    (compare (axis moon2) (axis moon1))))]))))

(defn apply-velocity [moons]
  (map
   (fn [moon]
     (-> moon
         (update :x (partial + (:vx moon)))
         (update :y (partial + (:vy moon)))
         (update :z (partial + (:vz moon)))))
   moons))

(defn total-energy [moons]
  (->> moons
       (map
        (fn [{:keys [x y z vx vy vz]}]
          (* (reduce + 0 (map abs [x y z]))
             (reduce + 0 (map abs [vx vy vz])))))
       (reduce +)))

(defn step [moons]
  (->> moons
       (apply-gravity)
       (apply-velocity)))

(defn answer [filename n]
  (->> (utils/read-input filename)
       (map parse-moon)
       (iterate step)
       (drop n)
       (first)
       (total-energy)))


(answer "2019/day12-example.txt" 10)
(answer "2019/day12-example2.txt" 100)
(answer "2019/day12.txt" 1000)

(ns advent2018.day23
  (:require [utils :as utils]))

(defn parse-nanobot [^String s]
  (let [[x y z r] (->> s
                       (re-matches #"^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)$")
                       (rest)
                       (map utils/parse-int))]
    {:coords [x y z] :radius r}))

(defn in-range? [nanobot1 nanobot2]
  (<=
   (utils/manhattan-distance (:coords nanobot1) (:coords nanobot2))
   (:radius nanobot1)))

(let [nanobots (map parse-nanobot (utils/read-input "2018/day23.txt"))
      max-bot (first (sort-by :radius > nanobots))]
  (->> nanobots
       (filter (partial in-range? max-bot))
       (count)))

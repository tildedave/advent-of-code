(ns advent2018.day23
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn parse-nanobot [^String s]
  (let [[x y z r] (->> s
                       (re-matches #"^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)$")
                       (rest)
                       (map utils/parse-int))]
    {:coords [x y z] :radius r :id (random-uuid)}))

(defn in-range? [nanobot1 nanobot2]
  (<=
   (utils/manhattan-distance (:coords nanobot1) (:coords nanobot2))
   (:radius nanobot1)))

(let [nanobots (map parse-nanobot (utils/read-input "2018/day23.txt"))
      max-bot (first (sort-by :radius > nanobots))]
  (->> nanobots
       (filter (partial in-range? max-bot))
       (count)))

;; so it seems like the approach we want is see which nanobots intersect which
;; other nanobot.
;; once we have this there we can find (?) a
;; maximal intersecting set of nanobots.
;; this is mutual, e.g. each nanobot has to intersect each other.
;; let's assume that - if we have that maximal intersecting set of nanobots -
;; finding the coords that are in range of them all is easy.
;; probably could be brute forced, though the numbers involved in my input are
;; quite large.

;; https://stackoverflow.com/questions/8367512/how-do-i-detect-intersections-between-a-circle-and-any-other-circle-in-the-same

(defn nanobots-intersect? [nanobot1 nanobot2]
  (<=
  ;;  (abs (- (:radius nanobot1) (:radius nanobot2)))
   (utils/manhattan-distance (:coords nanobot1) (:coords nanobot2))
   (+ (:radius nanobot1) (:radius nanobot2))))

;; find the nanobot with the most intersections, then compute the "full
;; intersection" of each nanobot with each other.
(defn gradient-descent-start [filename]
  (let [nanobots (map parse-nanobot (utils/read-input filename))
        by-id (reduce merge {} (map #(hash-map (:id %) %) nanobots))
        [start-id intersecting-ids] (->> (combo/combinations nanobots 2)
                                         (map #(if (apply nanobots-intersect? %)
                                                 {(:id (first %)) #{(:id (second %))}
                                                  (:id (second %)) #{(:id (first %))}}
                                                 nil))
                                         (remove nil?)
                                         (reduce (partial merge-with set/union) {})
                                         (sort-by #(count (second %)) >)
                                         (first))]
    [(:coords (by-id start-id))
     (map by-id (disj intersecting-ids start-id))]))


  ;; (println "nanobot with most intersections is" (first max-intersection) (by-id (first max-intersection))))

;; brute force for intersections probably is going to suck since the radius is so
;; large?  my example has max input radius 94 million.  all of them have 9 digit
;; radii. we can't enumerate points like this.
;; so the alternative is to try to have an object which represents the
;; "intersection" of two radii, but it is not clear what this looks like.
;; OK looking at reddit the term is an "axis-aligned bounding box", and we can
;; compute this, and we can compute their intersections.
;; we are going to be kind of stupider and try a gradient descent approach.

(defn total-distance-to-all [[x y z] nanobots]
  (->> nanobots
       (map #(utils/manhattan-distance [x y z] (:coords %)))
       (reduce +)))

(/ 5.5 2)

(defn gradient-descent [nanobots [x y z]]
  (let [dist (total-distance-to-all [x y z] nanobots)
        ;; we can probably step further at once, but let's start here.
        step-length 1] ;; (int (/ dist 4))]
    (->>
     (for [dx (range -1 2)
           dy (range -1 2)
           dz (range -1 2)]
       (if (= dx dy dz 0)
         nil
         [(+ x (* dx step-length))
          (+ y (* dy step-length))
          (+ z (* dz step-length))]))
     (remove nil?)
     ;; TODO: break ties by closeness to origin???
     (sort-by #(total-distance-to-all % nanobots))
     (first))))

(let [[start-coords nanobots] (gradient-descent-start "2018/day23-example.txt")]
  ;; (total-distance-to-all [0 0 0] nanobots))
  (iterate (partial gradient-descent nanobots) start-coords))



;; (abs (- r1 r2)) manhattan distance c1 c2 (abs (+ r1 r2))

;; the intersection of two nanobots is not a circle.

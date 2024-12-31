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

(defn nanobot-in-range? [nanobot1 nanobot2]
  (<=
   (utils/manhattan-distance (:coords nanobot1) (:coords nanobot2))
   (:radius nanobot1)))

(let [nanobots (map parse-nanobot (utils/read-input "2018/day23.txt"))
      max-bot (first (sort-by :radius > nanobots))]
  (->> nanobots
       (filter (partial nanobot-in-range? max-bot))
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

(defn in-range? [[x y z] nanobot]
  (<=
   (utils/manhattan-distance (:coords nanobot) [x y z])
   (:radius nanobot)))

(defn gradient-descent [nanobots [[x0 x1] [y0 y1] [z0 z1] step-size]]
  (if (zero? step-size)
    [[x0 x1] [y0 y1] [z0 z1] step-size]
    (let [[bx by bz]
          (->>
           (for [x (range x0 (inc x1) step-size)
                 y (range y0 (inc y1) step-size)
                 z (range z0 (inc z1) step-size)]
             [x y z])
           (map #(vector % (count (filter (partial in-range? %) nanobots))))
           (sort (fn [[coords1 n1] [coords2 n2]]
                      (if (not= n1 n2)
                        (compare n2 n1)
                        (compare (utils/manhattan-distance coords1 [0 0 0])
                                 (utils/manhattan-distance coords2 [0 0 0])))))
           (first)
           (first))]
      (if (= step-size 1)
        [[bx bx] [by by] [bz bz] 0]
        [[(- bx step-size) (+ bx step-size)]
         [(- by step-size) (+ by step-size)]
         [(- bz step-size) (+ bz step-size)]
         (quot step-size 2)]))))

;; (gradient-descent)
;;   (let [dist (total-distance-to-all [x y z] nanobots)
;;         ;; we can probably step further at once, but let's start here.
;;         step-length 1] ;; (int (/ dist 4))]
;;     (->>
;;      (for [dx (range -1 2)
;;            dy (range -1 2)
;;            dz (range -1 2)]
;;        (if (= dx dy dz 0)
;;          nil
;;          [(+ x (* dx step-length))
;;           (+ y (* dy step-length))
;;           (+ z (* dz step-length))]))
;;      (remove nil?)
;;      ;; TODO: break ties by closeness to origin???
;;      (sort-by #(total-distance-to-all % nanobots))
;;      (first))))

(* 2 2 2 2 2 2 2 2)

;; part 2 answer based on https://github.com/mfs/aoc-2018/blob/master/src/bin/p23.rs
;; my brain is just mush w/computational geometry
(let [[start-coords nanobots] (gradient-descent-start "2018/day23.txt")
      xs (map #(get-in % [:coords 0]) nanobots)
      [x0 x1] [(reduce min xs) (reduce max xs)]
      ys (map #(get-in % [:coords 1]) nanobots)
      [y0 y1] [(reduce min ys) (reduce max ys)]
      zs (map #(get-in % [:coords 2]) nanobots)
      [z0 z1] [(reduce min zs) (reduce max zs)]
      step-size (loop [step-size 1]
                  (if (>= step-size (- x1 x0))
                    step-size
                    (recur (* step-size 2))))
      _ (println step-size)
      _ (println x0 z1 y0 y1 z0 z1)]
  ;; (total-distance-to-all [0 0 0] nanobots))
  (->> (iterate (partial gradient-descent nanobots) [[x0 x1] [y0 y1] [z0 z1] step-size])
       (drop-while #(not (zero? (last %))))
       (first)
       (take 3)
       (map first)
       (reduce +)))

(ns advent2019.day10
  (:require [grid :as grid]
            [clojure.math.combinatorics :as combo]
            [clojure.math :as math]
            [utils :as utils]))

;; so the approach I used in golang for this was to uniquely identify an
;; asteroid by its angle.  we actually don't even need to arctan the coords
;; since the arctan is determined by the reduction to lowest terms and Clojure
;; just does that for us.

;; is there a smarter way?  angle between X and Y == -angle between Y and X.
;; whatever, it is n^2 time.

(defn asteroid-positions [grid]
  (->> (grid/coords grid)
       (filter (fn [coords] (= (grid/at grid coords) \#)))
       (set)))

(defn angle [[x1 y1] [x2 y2]]
  (math/atan2 (- y2 y1) (- x2 x1)))

(defn angles-to-other-asteroids [station asteroids]
  (as-> asteroids a
    (remove (partial = station) a)
    (group-by (partial angle station) a)
    (update-vals a #(->> %
                         (sort-by (partial utils/manhattan-distance station) >)
                         (vec)))))

;; part 1 answer doesn't actually require finding the closest asteroid :-)

(defn best-position [asteroids]
  (->>
   (for [station asteroids]
     [station (angles-to-other-asteroids station asteroids)])
   (sort-by #(count (second %)) >)
   (first)
   (first)))

(defn answer-part1 [file]
  (let [g (grid/parse-file file)
        asteroids (asteroid-positions g)]
    (-> (best-position asteroids)
        (angles-to-other-asteroids asteroids)
        (keys)
        (count))))

(answer-part1 "2019/day10.txt")

;; OK part 2 requires we start blowing stuff up
;; this DOES require finding the best spot.
;; OK we did that.  so then the logic is quite similar to
;; the golang solution.
;; it seems easiest to use iterate and output which stations
;; destroyed.

;; sorting the angles is a pain in the ass
;; so sort angle, find pi/2, chop anything before that off and concat to the end?

(defn order-angles [angles]
  (let [[before after]
        (->> angles
             (sort)
             (split-with (partial > (- (/ math/PI 2)))))]
    (into (vec after) (vec before))))

(defn destruction-order [file]
  (let [g (grid/parse-file file)
        asteroids (asteroid-positions g)]
    (iterate
     (fn [state]
       (reduce
        (fn [{:keys [system destroyed]} curr]
          (let [next-angle (pop (system curr))]
          ;; pop the last element, if empty
          {:destroyed (conj destroyed (peek (system curr)))
           :system (if (empty? next-angle)
                     (dissoc system curr)
                     (assoc system curr next-angle))}))
        (assoc state :destroyed [])
        (order-angles (keys (:system state)))))
     {:system (angles-to-other-asteroids (best-position asteroids) asteroids)
      :destroyed []})))

(defn answer-part2 [file]
  (->> (destruction-order file)
       (rest)
       (take-while #(seq (:destroyed %)))
       (mapcat :destroyed)
       (drop 199)
       (first)
       (map-indexed (fn [n x] (case n 0 (* 100 x) x)))
       (reduce +)))

;; angle sorting logic took longer than everything else :-)

(answer-part2 "2019/day10-example2.txt")
(answer-part2 "2019/day10.txt")

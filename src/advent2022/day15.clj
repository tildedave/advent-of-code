(ns advent2022.day15
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]))

(def lines (utils/read-resource-lines "input/day15-example.txt"))
(def sensor-re #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def coords
  (map
   #(let [[sx sy bx by] (->> %
                             (re-matches sensor-re)
                             (rest)
                             (map utils/parse-int))]
      [[sx sy] [bx by]])
   lines))

(def sensors-with-distances
  (map #(vector (first %) (apply manhattan-distance %)) coords))

(defn distance-from [x y sensors-with-distances]
  (map #(manhattan-distance (first %) [x y]) sensors-with-distances))

(map #(first %) sensors-with-distances)

(manhattan-distance (first (first sensors-with-distances)) [-3 10])

;; sort sensors by X axis.  have a left | right that we are maintaining.
;; need map from sensor to its distance.
;; so this lets us do the calculation in something like linear time.

(vector 1 2 3)
(defn beacon-elimination [y sensors-with-distances]
  ;; x needs to start at something ridiculously low and go to something
  ;; ridiculously high, essentially to make sure the distance is higher
  ;; than any beacon.
  ;; this a bit too cute, I bet a brute force approach would worked just as
  ;; well.
  (let [sensors-with-distances (vec sensors-with-distances)
        num-sensors (count sensors-with-distances)
        y-distances (into {} (map-indexed (fn [n [[_ sy]]] (vector n (abs (- y sy)))) sensors-with-distances))
        x-coords (into {} (map-indexed (fn [n [[sx _]]] [n sx]) sensors-with-distances))
        coords-to-x (set/map-invert x-coords)
        max-x 100]
    (loop [x -100
           distances (mapv #(+ (y-distances %) (abs (- x (x-coords %)))) (range num-sensors))
           num-possible 0]
      (if (> x max-x)
        num-possible
        (let [[next-distances is-possible]
              (reduce
               (fn [[next-distances is-possible] n]

                 )
               [distances true]
               (range num-sensors)

              ])
        (recur (inc x) next-distances next-possible)

      distances)))

(beacon-elimination 10 sensors-with-distances)

      ;; we expected these to be the same
      [coords-to-x
       (map #(+ (y-distances %) (abs (- x (x-coords %)))) (range num-sensors))
       (map #(manhattan-distance (first (nth sensors-with-distances %)) [x y]) (range num-sensors))])))
    ;; (loop [x -1000
    ;;        left {}
    ;;        right {}
    ;;        ]
    ;;   )
    ;; y-distances))


)

  (def sensors-with-distances
    (map #(vector (first %) (apply manhattan-distance %)) coords))



  (distance-from 100 10 sensors-with-distances))

  (- (count "..####B######################..") 4)

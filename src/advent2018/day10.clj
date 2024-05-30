(ns advent2018.day10
  (:require [utils :as utils]
            [clojure.math :as math]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.walk :refer [walk]]))

(defn parse-point [line]
  (->> line
       (re-matches #"^position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>$")
       (rest)
       (map utils/parse-int)
       ((fn [[px py vx vy]] {:px px :py py :vx vx :vy vy}))))

(defn step [particle]
  (let [{:keys [vx vy]} particle]
    (-> particle
        (update :px (partial + vx))
        (update :py (partial + vy)))))

(defn all-distance-mean [particles]
  (->> (combo/combinations particles 2)
       (map (fn [[p1 p2]] (math/sqrt (+ (math/pow (- (:px p1) (:px p2)) 2)
                                         (math/pow (- (:py p1) (:py p2)) 2)))))
       (utils/mean)))

(defn print-grid [particles]
  (let [px-min (walk :px #(reduce min %) particles)
        px-max (walk :px #(reduce max %) particles)
        py-min (walk :py #(reduce min %) particles)
        py-max (walk :py #(reduce max %) particles)
        coords (walk (fn [{:keys [px py]}] [px py]) set particles)]
  (println
   (string/join
   "\n"
   (for [y (range py-min (inc py-max))]
    (string/join (for [x (range px-min (inc px-max))]
      (if (contains? coords [x y]) \# \.))))))))

(defn apply-n [f n x]
  (if (zero? n)
    x
    (recur f (dec n) (f x))))
(+ 10500 211)
(->> (utils/read-input "2018/day10.txt")
     (map parse-point)
     (apply-n #(mapv step %) 10500)
     (iterate #(mapv step %))
     (map-indexed vector)
     (reduce
      (fn [[last last-mean] [n curr]]
        (let [m (all-distance-mean curr)]
          (if (> m last-mean)
            (reduced [n last])
            [curr m])))
      [nil Integer/MAX_VALUE])
     ((fn ([[n x]] (do (println (dec (+ 10500 n))) x))))
     (print-grid))

     (map-indexed (fn [n x] [n (all-distance-mean x)])))



;; (all-distance-mean )

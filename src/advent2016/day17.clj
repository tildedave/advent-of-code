(ns advent2016.day17
  (:require [utils :as utils]
            [grid :as grid]))


;; again we are doing A* search
;; again the distance is trivial (1)
;; however the state of a node is based on the path to get here.

(def directions '(:up :down :left :right))
(def dx-dy-map {:up [0 -1] :down [0 1] :left [-1 0] :right [1 0]})
(def direction-str {:up "U" :down "D" :left "L" :right "R"})

(defn neighbors [passcode]
  (memoize
   (fn [[[x y] path-str]]
     (let [can-move (->>
                     (subs (utils/md5-hex (format "%s%s" passcode path-str)) 0 4)
                     (seq)
                     (map-indexed (fn [^long n ch]
                                    {(case n 0 :up 1 :down 2 :left 3 :right)
                                     (case ch (\b \c \d \e \f) true false)}))
                     (reduce merge))]
       (->> (for [dir directions]
              (let [[dx dy] (dx-dy-map dir)
                    [nx ny] [(+ x dx) (+ y dy)]]
                (if (or (not (<= 0 nx 3)) (not (<= 0 ny 3)) (not (can-move dir)))
                  nil
                  [[nx ny] (str path-str (direction-str dir))])))
            (remove nil?))))))

((neighbors "hijkl") [[0 0] ""])
((neighbors "hijkl") [[0 1] "D"])

(def is-goal? (fn [[[x y] _]] (and (= x 3) (= y 3))))

(defn answer [passcode]
  (second (grid/a*-search
           [[0 0] ""]
           is-goal?
           (neighbors passcode)
           (fn [[[x y] _]] (+ (abs (- x 3)) (abs (- y 3))))
           (fn [_ _] 1))))

(answer "kglvqrro")
(answer (first (utils/read-input "2016/day17.txt")))

;; with deep sadness we must code more graph stuff.
;; basically run a queue and exhaust the search space.
;; hopefuly it isn't too bad :-)

(defn longest-path [passcode]
  (let [start [[0 0] ""]
        neighbors (neighbors passcode)]
    (loop [queue [start]
           paths-to-goal []]
      (if-let [x (first queue)]
        (let [queue (subvec queue 1)]
          (if (is-goal? x)
            (recur queue (conj paths-to-goal (second x)))
            (recur (reduce conj queue (neighbors x)) paths-to-goal)))
        paths-to-goal))))

(defn answer-part2 [passcode]
  (count (first (sort-by count > (longest-path passcode)))))

(answer-part2 "ihgpwlah")
(answer-part2 "kglvqrro")
(answer-part2 "ulqzkmiv")
(answer-part2 (first (utils/read-input "2016/day17.txt")))

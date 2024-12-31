(ns advent2018.day6
  (:require [utils :as utils]))

(defn closest-coord [coords]
  (memoize
   (fn [[x y]]
     (let [manhattan-distance (fn [[cx cy]] (+ (abs (- x cx)) (abs (- y cy))))
           closest (->> coords
                        (sort-by manhattan-distance)
                        (partition-by manhattan-distance)
                        (first))]
       (case (count closest)
         1 (first closest)
         nil)))))

;; part 1.  ugh
(let [coords (->> (utils/read-input "2018/day6.txt")
                  (map (fn [^String s] (map utils/parse-int (.split s ", ")))))
      closest-coord (closest-coord coords)
      coords-by-x (->> (sort-by first coords) (vec))
      coords-by-y (->> (sort-by second coords) (vec))
      [min-x max-x] [(first coords-by-x) (last coords-by-x)]
      [min-y max-y] [(first coords-by-y) (last coords-by-y)]
      ;; we could have used these coords-by-x and coords-by-y to
      ;; find closest coordinates ... probably.
      rect1 (frequencies
             (for [x (range min-x (inc max-x))
                   y (range min-y (inc max-y))]
               (closest-coord [x y])))
      _ (println rect1)
      rect2 (frequencies
             (for [x (range (- min-x 25) (+ max-x 26))
                   y (range (- min-y 25) (+ max-y 26))]
               (closest-coord [x y])))]
  (->> coords
       (remove #(not= (rect1 %) (rect2 %)))
       (map #(vector % (rect1 %)))
       (sort-by second >)
       (first)
       (second)))

;; part 2 seems easier
(defn answer-part2 [filename distance]
  (let [coords (->> (utils/read-input filename)
                    (map (fn [^String s] (map utils/parse-int (.split s ", ")))))
        coords-by-x (->> (sort-by first coords) (vec))
        coords-by-y (->> (sort-by second coords) (vec))
        [[min-x] [max-x]] [(first coords-by-x) (last coords-by-x)]
        [[_ min-y] [_ max-y]] [(first coords-by-y) (last coords-by-y)]
        ;; fudge of 0 actually works; no valid coords outside the bounds
        fudge 100]
    (reduce
     +
     0
     (for [x (range (- min-x fudge) (+ max-x fudge))
           y (range (- min-y fudge) (+ max-y fudge))]
       (let [manhattan-distance (fn [[cx cy]] (+ (abs (- x cx)) (abs (- y cy))))]
         (if (< (reduce + 0 (map manhattan-distance coords)) distance)
           1
           0))))))

(answer-part2 "2018/day6.txt" 10000)



      (defn closest-coord [coords]
        (memoize
         (fn [[x y]]
           (let [manhattan-distance (fn [[cx cy]] (+ (abs (- x cx)) (abs (- y cy))))
                 closest (->> coords
                              (sort-by manhattan-distance)
                              (partition-by manhattan-distance)
                              (first))]
             (case (count closest)
               1 (first closest)
               nil)))))
)
     ()
         ]))


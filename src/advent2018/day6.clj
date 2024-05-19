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
(let [coords (->> (utils/read-input "2018/day6-example.txt")
                  (map (fn [^String s] (map utils/parse-int (.split s ", ")))))
      closest-coord (closest-coord coords)
      coords-by-x (->> (sort-by first coords) (vec))
      coords-by-y (->> (sort-by second coords) (vec))
      [min-x _] (first coords-by-x)
      [max-x _] (last coords-by-x)
      [_ min-y] (first coords-by-y)
      [_ max-y] (last coords-by-y)
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

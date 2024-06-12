(ns advent2019.day3
  (:require [clojure.set :as set]
            [utils :as utils]))
(.substring "abc" 1)

(defn parse-commands [^String s]
  (->> (.split s ",")
       (map (fn [s]
              [(case (first s)
                 \R :right
                 \D :down
                 \U :up
                 \L :left)
               (utils/parse-int (.substring s 1))]))))

(defn step [{:keys [coords seen]}
            [direction num-steps]]
  (let [[x y] coords
        [dx dy] (case direction
                  :right [1 0]
                  :left [-1 0]
                  :up [0 -1]
                  :down [0 1])]
       {:coords [(+ x (* num-steps dx)) (+ y (* num-steps dy))]
        :seen
        (merge-with
        +
        seen
        (->> (range 1 (inc num-steps))
             (map (fn [n] [(* n dx) (* n dy)]))
             (map (fn [[dx dy]] {[(+ x dx) (+ y dy)] 1}))
             (reduce merge {})))}))

(defn answer [wire1 wire2]
  (let [coord-set (fn [wire] (->> wire
                                    (parse-commands)
                                    (reduce step {:coords [0 0] :seen {}})
                                    :seen
                                    (keys)
                                    (set)))]
    (->> (set/intersection (coord-set wire1) (coord-set wire2))
         (sort-by (partial utils/manhattan-distance [0 0]))
         (first)
         (utils/manhattan-distance [0 0]))))

(answer "R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83")

(answer "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

;; part 1
(apply answer (utils/read-input "2019/day3.txt"))

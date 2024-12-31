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
(conj '(1 2 3 4) 5)

(defn steps [[x y] command-list]
  (if-let [[direction num-steps] (first command-list)]
    (if (zero? num-steps)
      (steps [x y] (rest command-list))
      (let [[dx dy] (case direction
                      :right [1 0]
                      :left [-1 0]
                      :up [0 -1]
                      :down [0 1])
            [nx ny] [(+ x dx) (+ y dy)]]
        (cons
         [nx ny]
         (lazy-seq (steps [nx ny]
                          (cons
                           [direction (dec num-steps)]
                           (rest command-list)))))))
    '()
    ))

(steps [0 0] (parse-commands "R8,U5,L5,D3"))

(defn coord-set [wire]
  (->> wire
        (parse-commands)
        (steps [0 0])
        (set)))

(defn answer-part1 [wire1 wire2]
  (->> (set/intersection (coord-set wire1) (coord-set wire2))
       (sort-by (partial utils/manhattan-distance [0 0]))
       (first)
       (utils/manhattan-distance [0 0])))

(answer-part1 "R8,U5,L5,D3"
              "U7,R6,D4,L4")

(answer-part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
        "U62,R66,U55,R34,D71,R55,D58,R83")

(answer-part1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

;; part 1
(apply answer-part1 (utils/read-input "2019/day3.txt"))

(defn answer-part2 [wire1 wire2]
  (let [seen-coords (fn [wire] (->> wire
                                    (parse-commands)
                                    (steps [0 0])
                                    (map-indexed (fn [n x] [x (inc n)]))
                                    (into {})))
        seen-wire1 (seen-coords wire1)
        seen-wire2 (seen-coords wire2)]
    (->> (set/intersection (set (map first seen-wire1)) (set (map first seen-wire2)))
         (map (fn [coord] (+ (seen-wire1 coord)
                             (seen-wire2 coord))))
         (sort)
         (first))))

(answer-part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
              "U62,R66,U55,R34,D71,R55,D58,R83")

;; part 2
(apply answer-part2 (utils/read-input "2019/day3.txt"))

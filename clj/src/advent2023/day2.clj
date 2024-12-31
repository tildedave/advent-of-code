(ns advent2023.day2
  (:require [utils :as utils]
            [clojure.string :as string]))

(.split "Game 1: adjf adflajk" ": " 2)
(defn parse-bag [^String line]
  (as-> line s
    (.split s ": " 2)
    (second s)
    (.split s "; ") ;; split into individual games
    (map #(reduce merge {}
                  (map (fn [cube-and-num]
                         (let [[n color] (.split cube-and-num " " 2)]
                           {color (utils/parse-int n)}))
                       (.split % ", "))) s)))

(parse-bag "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(def example-record
  '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

;; part 1
(->>
 (map parse-bag example-record)
 (map-indexed vector)
 (filter (fn [[_ games]]
           (every?
            (fn [game-map]
              ;; we don't need to worry about non-blue-red-green bags.
              (and (<= (get game-map "red" 0) 12)
                   (<= (get game-map "green" 0) 12)
                   (<= (get game-map "blue" 0) 14)))
            games)))
 (map first)
 (map inc)
 (reduce +))

;; part 1
(defn answer-part1 [lines]
  (->>
   (map parse-bag lines)
   (map-indexed vector)
   (filter (fn [[_ games]]
             (every?
              (fn [game-map]
              ;; we don't need to worry about non-blue-red-green bags.
                (and (<= (get game-map "red" 0) 12)
                     (<= (get game-map "green" 0) 13)
                     (<= (get game-map "blue" 0) 14)))
              games)))
   (map first)
   (map inc)
   (reduce +)))

(answer-part1 (utils/read-input "2023/day2.txt"))

;; part 2
;; this is just reduce max on the entries I guess

(defn fewest-cubes [games]
  (reduce (partial merge-with max) games))

(defn power [cubes]
  (reduce * (vals cubes)))

(power (fewest-cubes (parse-bag "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")))

(defn answer-part2 [lines]
  (->>
   (map parse-bag lines)
   (map fewest-cubes)
   (map power)
   (reduce +)))

(answer-part2 example-record)
(answer-part2 (utils/read-input "2023/day2.txt"))

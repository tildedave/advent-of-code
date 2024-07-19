(ns advent2023.day18
  (:require [utils :as utils]))

;; ok so this is the shoelace formula
;; we will just do it with the shoelace formula immediately

(Integer/parseInt "70c710" 16)

(defn parse-instruction [line]
  (->> (re-matches #"^(R|D|L|U) (\d+) \(\#([0-9a-f]{5})([0-3])\)" line)
       (rest)
       ((fn [[dir num-str hex-string hex-direction]]
              [dir
               (parse-long num-str)
               (case hex-direction
                 "0" "R"
                 "1" "D"
                 "2" "L"
                 "3" "U")
               (Integer/parseInt hex-string 16)]))))

(def example-plan
  '("R 6 (#70c710)"
    "D 5 (#0dc571)"
    "L 2 (#5713f0)"
    "D 2 (#d2c081)"
    "R 2 (#59c680)"
    "D 2 (#411b91)"
    "L 5 (#8ceee2)"
    "U 2 (#caa173)"
    "L 1 (#1b58a2)"
    "U 2 (#caa171)"
    "R 2 (#7807d2)"
    "U 3 (#a77fa3)"
    "L 2 (#015232)"
    "U 2 (#7a21e3)"))

(map parse-instruction example-plan)

(defn coords [path]
  (loop [path path
         coord [0 0]
         result []]
    (if-let [[dir n] (first path)]
      (recur
       (rest path)
       (mapv
        +
        coord
        (case dir
          "U" [0 (- n)]
          "D" [0 n]
          "L" [(- n) 0]
          "R" [n 0]))
       (conj result coord))
      result)))

(coords (map parse-instruction example-plan))

;; first we zip
(defn shoelace-formula [coords]
  (let [l (count coords)]
    (/
     (->>
     (range 0 l)
     (map
      (fn [n]
        [(get coords n)
         (get coords (mod (inc n) l))]))
     (map
      (fn [[[x1 y1] [x2 y2]]]
        (- (* x1 y2) (* x2 y1))
        ))
     (reduce +))
     2)))

;; correct
(let [instructions (map parse-instruction (utils/read-input "2023/day18.txt"))
      perimeter (reduce + (map second instructions))]
  (+ (shoelace-formula (coords instructions))
     (/ perimeter 2)
     1))

(let [instructions (->> (utils/read-input "2023/day18.txt")
                        (map parse-instruction)
                        (map (partial drop 2)))
      perimeter (reduce + (map second instructions))]
  (+ (shoelace-formula (coords instructions))
     (/ perimeter 2)
     1))

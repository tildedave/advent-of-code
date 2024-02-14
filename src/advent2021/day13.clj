(ns advent2021.day13
  (:require [advent2021.utils :as utils]
            [clojure.string :as string]))

(defn parse-instruction [str]
  (let [[_ coord num-str] (re-matches #"^fold along (x|y)=(\d+)$"  str)]
    [coord (utils/parse-int num-str)]))

(parse-instruction "fold along y=7")

(defn parse-lines [lines]
  (let [[coords instrs] (->> lines
                             (partition-by #(= % ""))
                             (remove #(= % (list ""))))]
    (list (->> coords
               (map #(.split % ","))
               (map #(mapv utils/parse-int %))
               (set))
          (map parse-instruction instrs))))

(defn bounds [coords]
  [(->> coords
        (map first)
        (reduce max)
        (inc))
   (->> coords
        (map second)
        (reduce max)
        (inc))])

(->> (utils/read-resource-lines "input/day13-example.txt")
     (parse-lines))

(defn fold [coords [axis fold-line]]
  ;; we fold horizontal lines UP
  ;; we fold vertical lines LEFT
  ;; so we partition the coords based on their axis
  (let [idx (case axis "x" 0 "y" 1)
        [orig-points fold-points] (utils/sml-partition #(< (get % idx) fold-line) coords)]
    (->> fold-points
         (map (fn [[x y]]
                (case axis
                  "x" [(+ fold-line (- fold-line x)) y]
                  "y" [x (+ fold-line (- fold-line y))])))
         (concat orig-points)
         (set))))

(defn answer-part1 [lines]
  (let [[coords instr] (parse-lines lines)]
    (->> coords
         (fold (first instr))
         (count))))

(answer-part1 (utils/read-resource-lines "input/day13-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day13.txt"))

(defn draw-dots [coords]
  (let [[xmax ymax] (bounds coords)]
    (string/join
     "\n"
     (for [y (range 0 ymax)]
      (string/join
       (for [x (range 0 xmax)]
        (if (contains? coords [x y])
          \#
          \.)))))))

(defn answer-part2 [lines]
  (let [[coords instr] (parse-lines lines)]
    (->> (reduce fold coords instr)
         (draw-dots)
         (println))))

(answer-part2 (utils/read-resource-lines "input/day13.txt"))

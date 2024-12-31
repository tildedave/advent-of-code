(ns advent2019.day8
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn layers [w h seq]
  (partition h (partition w seq)))

(defn num-ones [layer]
  (count (filter zero? (flatten layer))))

(layers 3 2 '(1 2 3 4 5 6 7 8 9 0 1 2))

(defn answer-part1 [layer]
  (let [res (group-by identity (flatten layer))]
    (* (count (res 1)) (count (res 2)))))


;; part 1 answer
(->> (utils/read-input "2019/day8.txt")
     (first)
     (seq)
     (map #(utils/parse-int (str %)))
     (layers 25 6)
     (sort-by num-ones)
     (first)
     (answer-part1))

(defn pixel-result
  "p1 is in front of p2"
  [p1 p2]
  (case p1
    2 p2
    1 1
    0 0))

;; part 2
(->> (utils/read-input "2019/day8.txt")
     (first)
     (seq)
     (map #(utils/parse-int (str %)))
     (layers 25 6)
     ;; no need to supply an initial
     (reduce
      (fn [layer current-layer]
        (map (fn [row current-row]
               (map pixel-result row current-row))
             layer current-layer)))
     (map #(map (fn [n] (case n 1 \# \.)) %))
     (map string/join)
     (string/join "\n")
     (println))

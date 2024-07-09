(ns advent2023.day8
  (:require [utils :as utils]))

(defn parse-line [line]
  (->> line
       (re-matches #"^(\p{Upper}{3}) = \((\p{Upper}{3}), (\p{Upper}{3})\)$")
       (rest)
       ((fn [[first left right]] {first [left right]}))))

(defn parse-input [line]
  (let [[[instructions] transitions] (utils/split-by "" line)]
    [(vec instructions) (into {} (map parse-line transitions))]))

(def example-lines '("RL"
   ""
   "AAA = (BBB, CCC)"
   "BBB = (DDD, EEE)"
   "CCC = (ZZZ, GGG)"
   "DDD = (DDD, DDD)"
   "EEE = (EEE, EEE)"
   "GGG = (GGG, GGG)"
   "ZZZ = (ZZZ, ZZZ)"))

(defn answer-part1 [lines]
  (let [[instructions transitions] (parse-input lines)]
    (->> [(cycle (seq instructions)) "AAA"]
     (iterate
     (fn [[instructions current]]
       [(rest instructions)
        (nth (transitions current) (case (first instructions)
                                     \L 0
                                     \R 1))]))
         (map-indexed vector)
         (drop-while #(not= (second (second %)) "ZZZ"))
         (first)
         (first))))

;; easy
(answer-part1 example-lines)
(answer-part1 (utils/read-input "2023/day8.txt"))

(defn walk-cycle [instructions transitions starting-location]
  (iterate
   (fn [[n location]]
     [(mod (inc n) (count instructions))
      (nth (transitions location) (case (get instructions n)
                                    \L 0
                                    \R 1))])
   [0 starting-location]))

(defn z-cycle-length [walk-seq]
  (->> walk-seq
       (map-indexed vector)
       (reduce
        (fn [acc [n current]]
                 (if (and (contains? acc current) (.endsWith (second current) "Z"))
                    ;;  (println acc current n)
                   (reduced
                    {:start (acc current)
                    :length (- n (acc current))})
                   (assoc acc current n)))
        {})
       ((fn [answer]
          ;; this is not guaranteed, it just happens to be true.
          (assert (.endsWith (second (nth walk-seq (:length answer))) "Z"))
          answer))))

(defn cycles [lines]
  (let [[instructions transitions] (parse-input lines)
        starting-points (->> (keys transitions) (filter #(.endsWith % "A")))]
    ;; how long does it take to reach a Z at a certain point in the sequence
    ;; I believe they end up at Z at the end of the sequence which is a
    ;; kindness.
    ;; map-indexed + find cycle for Z.
    (->> starting-points
         (map #(z-cycle-length (walk-cycle instructions transitions %)))
         (map :length)
         (reduce utils/lcm))))

(def example-part2
  '("LR"
    ""
    "QQA = (QQB, XXX)"
    "QQB = (XXX, QQZ)"
    "QQZ = (QQB, XXX)"
    "SSA = (SSB, XXX)"
    "SSB = (SSC, SSC)"
    "SSC = (SSZ, SSZ)"
    "SSZ = (SSB, SSB)"
    "XXX = (XXX, XXX)"))

(cycles example-part2)
(cycles (utils/read-input "2023/day8.txt"))

;; this is the answer
(reduce utils/lcm '(16343 20221 19667 21883 13019 11911))

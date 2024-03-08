(ns advent2021.day3
  (:require [utils :as utils]
            [clojure.string :as string]))

(defn count-occurrences [lines]
  (reduce
   (fn [m line]
     (reduce
      (fn [m [n ch]]
        (update-in m [n ch] (fnil inc 0)))
      m
      (->> line
           (seq)
           (map-indexed vector))))
   {}
   lines))

(defn max-occurrence [m]
  (case  (compare (m \0) (m \1))
    0 \1
    1 \0
    -1 \1))

(defn least-occurrence [m]
  (case (compare (m \0) (m \1))
    0 \0
    1 \1
    -1 \0))

(defn answer-part1 [lines]
  (let [occ (->> lines
                 (count-occurrences)
                 (sort-by first))
        gamma (->> occ
                   (map #(max-occurrence (second %)))
                   (string/join))
        epsilon (->> occ
                     (map #(least-occurrence (second %)))
                     (string/join))]
    (->> [gamma epsilon]
         (map #(Integer/parseInt % 2))
         (reduce *)
    )))

        ;;  (reduce *))))

(Integer/parseInt "10110" 2)

(answer-part1 (utils/read-resource-lines "input/day3-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day3.txt"))

;; get the occurrences, use it to winnow down the list, stop when we get to one.

(defn part2-filter [lines f]
  (loop [lines lines
         n 0]
    (let [occ (count-occurrences lines)]
      (if (= (count lines) 1) (first lines)
      ;; otherwise, look at position n, see which is MORE common, use it to
      ;; winnow.
          (let [x (f (occ n))]
            (recur
             (filter #(= (get % n) x) lines)
             (inc n)))))))

(defn part2-answer [lines]
  (->>
   [(part2-filter lines max-occurrence)
    (part2-filter lines least-occurrence)]
   (map #(Integer/parseInt % 2))
   (reduce *)))

(part2-answer (utils/read-resource-lines "input/day3-example.txt"))
(part2-answer (utils/read-resource-lines "input/day3.txt"))

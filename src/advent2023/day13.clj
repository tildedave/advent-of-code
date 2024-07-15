(ns advent2023.day13
  (:require [grid :as grid]
            [utils :as utils]))

;; I did this one quite quickly in Golang using bit masks
;; bit masks are I suppose the best approach here since
;; once you read the numbers in as rows/columns all the rest
;; of your operations are O(1)
;;
;; OK, here we go.

(def ^:dynamic part2? false)

(grid/parse
 '("#.##..##."
   "..#.##.#."
   "##......#"
   "##......#"
   "..#.##.#."
   "..##..##."
   "#.#.##.#."))

(defn to-mask [line]
  (->> line
       (map #(case % \# 1 \. 0))
       (reduce (fn [acc n]
                 (+ (bit-shift-left acc 1) n)))))

(sort-by first {2 65 1 43 0 23})

(defn summarize [grid]
  (let [coords (grid/coords grid)]
    {:vertical (->>
                (update-vals
                 (group-by first coords)
                 (fn [coords] (->> coords
                                   (map #(grid/at grid %))
                                   (to-mask))))
                (sort-by first)
                (mapv second))
     :horizontal (->>
                  (update-vals
                   (group-by second coords)
                   (fn [coords] (->> coords
                                     (map #(grid/at grid %))
                                     (to-mask))))
                  (sort-by first)
                  (mapv second))}
     ))

(map to-mask (grid/parse
              '("#.##..##."
                "..#.##.#."
                "##......#"
                "##......#"
                "..#.##.#."
                "..##..##."
                "#.#.##.#.")))

(defn palindrome-at?
  "True if 0...n+1 = n+1...end, with non-matching coords not checked."
  [v n]
  (every? true?
          (map =
               (rseq (subvec v 0 (inc n)))
               (subvec v (inc n)))))

(defn smudged-palindrome-at?
  [v n]
  (let [res (remove zero? (map bit-xor
                               (rseq (subvec v 0 (inc n)))
                               (subvec v (inc n))))]
    (and
     (= (count res) 1)
     (zero? (bit-and (first res) (dec (first res)))))))

(defn note-score [summary]
  (let [find-summary (fn [v multiplier]
                       (if-let [x (->>
                                   (range 0 (dec (count v)))
                                   (filter
                                    (partial (if part2?
                                               smudged-palindrome-at?
                                               palindrome-at?) v))
                                   (first))]
                         (* (inc x) multiplier)
                         0))]
    (->> (list)
         (cons (find-summary (:vertical summary) 1))
         (cons (find-summary (:horizontal summary) 100))
         (remove zero?)
         (first))))

(defn score [lines]
  (->> lines
       (grid/parse)
       (summarize)
       (note-score)))

(defn answer-part1 [lines]
  (->> lines
       (utils/split-by "")
       (map score)
       (reduce +)))

(answer-part1 '("#.##..##."
                "..#.##.#."
                "##......#"
                "##......#"
                "..#.##.#."
                "..##..##."
                "#.#.##.#."
                ""
                "#...##..#"
                "#....#..#"
                "..##..###"
                "#####.##."
                "#####.##."
                "..##..###"
                "#....#..#"))

;; correct
(answer-part1 (utils/read-input "2023/day13.txt"))

;; also correct
(binding [part2? true] (answer-part1 (utils/read-input "2023/day13.txt")))

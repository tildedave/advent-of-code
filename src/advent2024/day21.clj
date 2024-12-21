(ns advent2024.day21
  (:require [graph :as graph]
            [grid :as grid]
            [clojure.string :as string]
            [utils :as utils]))

;; you can feel the puzzle creator laughing at us
;; I think this isn't so bad

(def button-grid
  [[\7 \8 \9]
   [\4 \5 \6]
   [\1 \2 \3]
   [\# \0 \A]])

(def directional-grid
  [[\# \^ \A]
   [\< \v \>]])

(defn paths [grid source dest]
  (graph/all-paths
   (grid/coords-of grid source)
   (grid/coords-of grid dest)
   (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))))

(paths button-grid \7 \A)

(defn any-path [grid source dest]
  (first (paths grid source dest)))

(defn path-to-keypresses [path]
  (loop [x (first path)
         xs (rest path)
         presses []]
    (if-let [next (first xs)]
      (recur
       next
       (rest xs)
       (conj
        presses
        (case (mapv - next x)
          [0 -1] \^
          [0 1] \v
          [1 0] \>
          [-1 0] \<)))
      (conj presses \A))))

(path-to-keypresses (any-path button-grid \A \0))

(def code-to-paths
  (memoize
   (fn [grid code]
  (loop [curr \A
         code-seq (seq code)
         all-paths [[]]]
    (if-let [next (first code-seq)]
      (recur
       next
       (rest code-seq)
       (let [next-paths (map path-to-keypresses (paths grid curr next))]
         (apply concat
                (for [path all-paths]
           (map
            #(into path %)
            next-paths)))))
      (set (map string/join all-paths)))))
))

(reduce min (map count
     (mapcat (partial code-to-paths directional-grid)
     (mapcat (partial code-to-paths directional-grid)
          (code-to-paths button-grid "029A")))))

(defn shortest-sequence-length [code]
  (->> (code-to-paths button-grid code)
       (mapcat (partial code-to-paths directional-grid))
       (mapcat (partial code-to-paths directional-grid))
       (map count)
       (reduce min)))

(defn numeric-part [code]
  (->> (seq code)
       (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
       (string/join)
       (parse-long)))

(numeric-part "029A")
(shortest-sequence-length "029A")

(defn answer-part1 [codes]
  (->> codes
       (map #(do (println %)
                 (* (shortest-sequence-length %) (numeric-part %))))
       (reduce +)))

(answer-part1 '("029A" "980A" "179A" "456A" "379A"))
(println "answer" (answer-part1 (utils/read-input "2024/day21.txt")))

;; it looks like we do care about "all paths" since different
;; sequences of keypresses require different movements of the robot-arm

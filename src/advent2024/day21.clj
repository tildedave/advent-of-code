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

(defn invalid-path? [grid path]
  (contains? (set (map (partial grid/at grid) path)) \#))

(def paths (memoize (fn [grid source dest]
  (let [[sx sy] (grid/coords-of grid source)
        [dx dy] (grid/coords-of grid dest)
        x-direction (compare dx sx)
        y-direction (compare dy sy)]
    ;; can either go left / up or up / left &c
    ;; but can't go to 0, 3 or 0, 0
    (->>
     (list
     (concat
      (for [x (range sx (+ dx x-direction) x-direction)]
        [x sy])
      (for [y (range sy (+ dy y-direction) y-direction)]
        [dx y]))
     (concat
      (for [y (range sy (+ dy y-direction) y-direction)]
        [sx y])
      (for [x (range sx (+ dx x-direction) x-direction)]
        [x dy])))
     (distinct)
     (remove (partial invalid-path? grid))
     )))))

(defn paths-old [grid source dest]
  (graph/all-paths
   (grid/coords-of grid source)
   (grid/coords-of grid dest)
   (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))))

(paths button-grid \7 \9)
(paths-old button-grid \7 \9)
(paths button-grid \2 \9)
(paths-old button-grid \2 \9)

;;   (graph/all-paths
;;    (grid/coords-of grid source)
;;    (grid/coords-of grid dest)
;;    (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))))

(paths button-grid \2 \9)

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
       (if (= next x)
         presses
         (conj
          presses
          (case (mapv - next x)
            [0 -1] \^
            [0 1] \v
            [1 0] \>
            [-1 0] \<))))
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

(println
 (reduce min (map count
     (mapcat (partial code-to-paths directional-grid)
     (mapcat (partial code-to-paths directional-grid)
          (code-to-paths button-grid "029A"))))))

;; we will try a greedy approach.  (it might work!)
;; greedy seems to work but it doesn't seem to prune as much as I need

(defn number-changed-presses [path]
  (reduce + (map (fn [ch1 ch2] (if (= ch1 ch2) 0 1)) (rest path) path)))

(number-changed-presses "<A^A>^^AvvvA")

(defn prune-paths [paths]
  (->> paths
       (sort (fn [path1 path2]
               (compare (number-changed-presses path1) (number-changed-presses path2))))
       (partition-by number-changed-presses)
       (first)))

(defn shortest-sequence-length [code num-indirections prune?]
    (loop [paths (code-to-paths button-grid code)
           n 0]
      (if (= n num-indirections)
        (reduce min (map count paths))
        (recur
         (let [next-paths (mapcat (partial code-to-paths directional-grid) paths)]
          (if prune? (prune-paths next-paths) next-paths))
         (inc n)))))

(time (println "prune" (shortest-sequence-length "379A" 2 false)))

(defn numeric-part [code]
  (->> (seq code)
       (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
       (string/join)
       (parse-long)))

(numeric-part "029A")
(shortest-sequence-length "029A" 2 false)

(defn answer-part1 [codes]
  (->> codes
       (map #(do (println %)
                 (* (shortest-sequence-length % 2 true) (numeric-part %))))
       (reduce +)))

(answer-part1 '("029A" "980A" "179A" "456A" "379A"))
(println "answer" (answer-part1 (utils/read-input "2024/day21.txt")))

;; OK so part 2 is what I was expecting
;; we will need some level of pruning after each level
;; another idea would be to make "all paths" so we only ever go in
;; straight lines

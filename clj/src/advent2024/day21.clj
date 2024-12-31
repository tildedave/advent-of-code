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

;; of course not all paths are good ones, we will prune later.
(defn all-paths [grid source dest]
  (graph/all-paths
   (grid/coords-of grid source)
   (grid/coords-of grid dest)
   (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))))

;; for this to be solvable EITHER there is a greedy approach and only ONE
;; transition at each branching point (up to my 33 million length sequence)
;; or a trick about using numbers.  I kind of doubt the numbers can work.
;; we already have seen that we can prune certain "bad" digit manipulations

(all-paths button-grid \7 \9)
(all-paths button-grid \2 \9)

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

(def next-codes
  (memoize
   (fn [grid code path-expander]
     (loop [curr \A
            code-seq (seq code)
            paths [[]]]
       (if-let [next (first code-seq)]
         (recur
          next
          (rest code-seq)
          (let [next-paths (map path-to-keypresses (path-expander grid curr next))]
            (apply concat
                   (for [path paths]
                     (map
                      #(into path %)
                      next-paths)))))
         (set (map string/join paths)))))))

(next-codes button-grid "029A" all-paths)

(def paths-smart
  (memoize
   (fn [grid source dest]
     (->> (all-paths grid source dest)
          (map (fn [path]
                 (let [codes (next-codes directional-grid (path-to-keypresses path) all-paths)]
                   (vector path
                           (string/join (path-to-keypresses path))
                           (map
                            (fn [path] (next-codes directional-grid path all-paths))
                            codes)))))
          (sort-by #(count (first (first (nth % 2)))))
          (partition-by #(count (first (first (nth % 2)))))
          (first)
          (map first)
          ;; it turns out any ambiguity at this point doesn't matter.
          (take 1)))))

(next-codes directional-grid (first (next-codes button-grid "029A" paths-smart)) paths-smart)

(next-codes directional-grid ">^" paths-smart)
(next-codes directional-grid "^>" paths-smart)
(next-codes directional-grid "Av" paths-smart)
(next-codes directional-grid "vA" paths-smart)

;; only ^>, >^ , vA, Av have a difference in how to do it optimally
;; all others are totally determined
;; everything has to return to A; once we return to A we don't care about the
;; string's prefix.
;; let's see if we can understand a bit more about the problem

;; idea: same technique from stones.
;; question: we do have to follow tree branches and do some kind of cutoff
;; I think we need to assume yes, but let's see
;; good news, buttons paths have an unambiguous "best" path between them always

(->>
 (for [x '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A)
      y '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A)]
  [[x y]
   (if (= x y)
     \A
     (map #(string/join (path-to-keypresses %)) (paths-smart button-grid x y)))])
 (filter (fn [[[x y] _]]
           (case [x y]
                       [\2 \4] true
                       [\4 \6] true
                       [\6 \A] true
                       false))))

;; good news, buttons in my input have an unambiguous "best" path between them
;; bad news, not sure what to do about the ambiguity for \v \A

(defn split-by-as [seq]
  (loop [curr []
         splits []
         seq seq]
    (if-let [x (first seq)]
      (if (= x \A)
        (recur [] (conj splits (conj curr x)) (rest seq))
        (recur (conj curr x) splits (rest seq)))
      (mapv string/join splits))))

(split-by-as '(\^ \A \< \< \^ \^ \A \A \A \> \> \A \v \v \v \A))

(defn next-substrings [grid code]
  (split-by-as
   (first (next-codes grid code paths-smart))))

(next-codes button-grid "029A" paths-smart)
(next-substrings button-grid "379A")
(next-codes button-grid (str \A "379A") paths-smart)

(defn count-reducer [counts [code n]]
  (as-> code c
    (next-substrings directional-grid c)
    (frequencies c)
    (update-vals c (partial * n))
    (merge-with + counts c)))

(defn code-sequence [code]
  (iterate
   (fn [counts]
     (reduce
      count-reducer
      {}
      counts))
   (frequencies (next-substrings button-grid code))))

(defn total-length [m]
  (reduce + (map (fn [[k v]] (* (count k) v)) m)))

(defn numeric-part [code]
  (->> (seq code)
       (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
       (string/join)
       (parse-long)))

(defn minimum-button-pushes [codes n]
  (->> codes
       (map #(* (total-length (nth (code-sequence %) n)) (numeric-part %)))
       (reduce +)))

(assert (= (minimum-button-pushes '("029A" "980A" "179A" "456A" "379A") 2) 126384))
(println "answer - part 1" (minimum-button-pushes (utils/read-input "2024/day21.txt") 2))
(println "answer - part 2" (minimum-button-pushes (utils/read-input "2024/day21.txt") 25))

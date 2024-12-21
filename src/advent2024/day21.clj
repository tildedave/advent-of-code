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
                         (remove (partial invalid-path? grid)))))))

(defn all-paths [grid source dest]
  (graph/all-paths
   (grid/coords-of grid source)
   (grid/coords-of grid dest)
   (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))))

;; for this to be solvable EITHER there is a greedy approach and only ONE
;; transition at each branching point (up to my 33 million length sequence)
;; or a trick about using numbers.  I kind of doubt the numbers can work.
;; we already have seen that we can prune certain "bad" digit manipulations

(paths button-grid \7 \9)
(all-paths button-grid \7 \9)
(paths button-grid \2 \9)
(all-paths button-grid \2 \9)

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
         (set (map string/join all-paths)))))))

(->> (all-paths directional-grid \A \v)
     (map #(code-to-paths directional-grid (path-to-keypresses %)))
    ;;  (map #(map count %)))
)

(def paths-smart
  (memoize
   (fn [grid source dest]
     (->> (all-paths grid source dest)
          (map (fn [path]
                 (let [next-paths (code-to-paths directional-grid (path-to-keypresses path))]
                 (vector path
                         (string/join (path-to-keypresses path))
                         (map
                          (fn [path] (code-to-paths directional-grid path))
                          next-paths
                          )))))
          (sort-by #(count (first (first (nth % 2)))))
          (partition-by #(count (first (first (nth % 2)))))
          (first)
          (map first)
          ))))

(paths-smart button-grid \3 \7)
(all-paths directional-grid \A \v)
;; these all have an unambiguous winner
(paths-smart directional-grid \A \v)
(paths-smart directional-grid \^ \>)
(paths-smart directional-grid \> \^)
;; this one is actually ambiguous as to which to do
;; kind of makes sense
(paths-smart directional-grid \v \A)


;; only ^>, >^ , vA, Av have a difference in how to do it optimally
;; all others are totally determined
;; everything has to return to A; once we return to A we don't care about the
;; string's prefix.
;; let's see if we can understand a bit more about the problem
;; ;; we basically never want to
;; (paths-smart directional-grid \A \v)

;;           (sort-by #(code-to-paths directional-grid %))
;;           (partition-by (path-to-keypresses path))))))

(defn pairs [l]
  (map vector l (rest l) ))

;; idea: same technique from stones.
;; question: we do have to follow tree branches and do some kind of cutoff
;; I think we need to assume yes, but let's see
;; good news, buttons are unambiguous
(defn paths-from-seq [grid source dest]
  (->> (paths-smart grid source dest)
       (map (fn [path] (let [presses (path-to-keypresses path)]
                         (if (= presses '(\A))
                           [[\A \A]]
                           (pairs presses)))))))

(paths-from-seq directional-grid \^ \^)

(paths-from-seq button-grid \3 \7)
(paths-from-seq directional-grid \v \A)

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

(defn count-reducer [grid]
  (fn [counts [[source dest] v]]
    (println source dest v)
    (let [best-paths (paths-from-seq grid source dest)
          _ (println "best path between" source dest "is" (first best-paths))]
      (merge-with + counts (update-vals
                            (frequencies (first best-paths))
                            (partial * v))))))

(defn code-sequence [code]
  (iterate
   (fn [counts]
     (reduce
      (count-reducer directional-grid)
      {}
      counts))
   (reduce
    (count-reducer button-grid)
    {}
    (frequencies (pairs code)))))

(take 2 (code-sequence "379A"))

;; this is a set of CFG transitions
;; it seems like each sequence gets twice as big
;; so in the end 2^25 is length 33 million
;; that is likely also impossible to do via direct attack, so there is some
;; trick about sequence lengths.
;; let's try some direct search for fun, maybe I will learn something about
;; the problem
;; I did not learn anything about the problem

(for [x '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A)
      y '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A)]
  [[x y]
   (if (= x y)
     \A
     (map #(string/join (path-to-keypresses %)) (paths button-grid x y)))])

(->>
 (for [x '(\< \^ \> \v \A)
      y '(\< \^ \> \v \A)]
  [(str x y)
   (if (= x y)
     (list \A)
     (map #(string/join (path-to-keypresses %)) (paths directional-grid x y)))])
 (remove (fn [[[x y] _]] (case [x y]
                           [\> \<] true
                           [\< \>] true
                           [\v \^] true
                           [\^ \v] true
                           false))))

(defn count-a [str]
  (->> (seq str) (filter (partial = \A)) (count)))

(count-a ">A^^^A")


(defn all-sequences [code num-indirections]
  (loop [paths (code-to-paths button-grid code)
         n 0]
    (if (= n num-indirections)
      (sort-by count paths)
      (recur
       (mapcat (partial code-to-paths directional-grid) paths)
       (inc n)))))

(println (string/join "\n" (all-sequences "379A" 1)))

(println (string/join "\n" (all-sequences "379A" 2)))

;; so
(defn shortest-sequence-search [code num-indirections]
  (loop [candidates (mapv
                     (fn [path] {:generation 0 :code path})
                     (code-to-paths button-grid code))
         current-bests {}]
    (if-let [{:keys [generation code]} (first candidates)]
      (let [a (count code)
            candidates (rest candidates)
            current-bests (update current-bests generation (fnil min Integer/MAX_VALUE) a)]
        (cond
          (> a (get current-bests generation Integer/MAX_VALUE))
          (recur candidates current-bests)
          (= generation num-indirections)
          (recur candidates current-bests)
          :else
          (recur
           (reduce
            (fn [candidates code]
              (if (> (count code) (get current-bests (inc generation) Integer/MAX_VALUE))
                candidates
                (conj candidates {:generation (inc generation) :code code})))
            candidates
            (code-to-paths directional-grid code))
           current-bests)))
      current-bests)))

(time (shortest-sequence-search "379A" 3))
(time (println "prune" (shortest-sequence-length "379A" 2 false)))
(time (println "prune" (shortest-sequence-length "379A" 2 true)))

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
                 (* (shortest-sequence-length % 2 false) (numeric-part %))))
       (reduce +)))

(answer-part1 '("029A" "980A" "179A" "456A" "379A"))
(println "answer" (answer-part1 (utils/read-input "2024/day21.txt")))

(defn answer-part2 [codes]
  (->> codes
       (map #(do (println %)
                 (* (shortest-sequence-length % 25 false) (numeric-part %))))
       (reduce +)))

(answer-part2 '("029A" "980A" "179A" "456A" "379A"))

;; OK so part 2 is what I was expecting
;; we will need some level of pruning after each level
;; another idea would be to make "all paths" so we only ever go in
;; straight lines

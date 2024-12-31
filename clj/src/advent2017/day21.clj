(ns advent2017.day21
  (:require [utils :as utils]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

;; OK 3x3 bitsets can be flipped, rotated, etc.
;; is it right to use a bitset?  meh, I love bitsets.
;; let's try without a bitset.
;; we can turn it back into a bitset pretty easily.

(defn rotate-right [x]
  (case (count x)
    3 (let [[[x1 x2 x3] [x4 x5 x6] [x7 x8 x9]] x]
        [[x7 x4 x1]
         [x8 x5 x2]
         [x9 x6 x3]])
    2 (let [[[x1 x2] [x4 x5]] x]
        [[x4 x1]
          [x5 x2]])))

(defn rotate-left [x] (-> x (rotate-right) (rotate-right) (rotate-right)))

(defn flip-vertical [x]
  (case (count x)
    3 (let [[first-line second-line third-line] x]
        [third-line second-line first-line])
    2 (let [[first-line second-line] x] [second-line first-line])))

(defn flip-horizontal [x]
  (case (count x)
    3 (let [[[[x1 x2 x3] [x4 x5 x6] [x7 x8 x9]]] x]
        [[x3 x2 x1]
         [x6 x5 x4]
         [x9 x8 x7]])
    2 (let [[[x1 x2] [x3 x4]] x]
        [[x2 x1] [x4 x3]])))

(defn parse-block [^String s]
  (mapv vec (.split s "/")))

(defn parse-pattern [^String s]
  (apply hash-map (map parse-block (.split s " => "))))

(rotate-left (parse-block ".../#.#/..#"))

(def starting-pattern [[\. \# \.] [\. \. \#] [\# \# \#]])

;; split the grid into 2 or 3 pieces.
;; we need these to actually be ordered.
;; to reconstruct them we will need to know where the line breaks are.
;; it is easier to me to just have this return a list of lists.
(range 0 4 2)

(defn split-into [grid n]
  (for [y (range 0 (count grid) n)]
    (for [x (range 0 (count grid) n)]
      (vec (for [dy (range 0 n)]
             (subvec (grid (+ y dy)) x (+ x n)))))))

;; woo-hoo!
(split-into [[\# \. \. \#] [\. \. \. \.] [\. \. \. \.] [\# \. \. \#]] 2)

(partition 1 '(1 2 3 4))

(defn reconstruct [grid-pieces]
  (->>
   (for [row grid-pieces]
     (let [irow (apply interleave row)]
       (->> irow
            (partition (quot (count irow) (count (first row))))
              ;; this combines all the lines into the correct order, but we
              ;; need to break it back up again (e.g. we have to insert a
              ;; newline)
              ;; (partition )
            (map #(reduce into [] %)))))
   (apply concat)
   (vec)))

(reconstruct (split-into [[\# \. \. \#] [\. \. \. \.] [\. \. \. \.] [\# \. \. \#]] 2))
(reconstruct (split-into starting-pattern 3))

(split-into starting-pattern 3)

(defn substitute-for-pattern [patterns block]
  (loop [block block
         n 0]
    (cond
      (= n 4) (throw (Exception. (format "Could not find pattern for %s" block)))
      (contains? patterns block) (patterns block)
      (contains? patterns (flip-vertical block)) (patterns (flip-vertical block))
      :else (recur (rotate-right block) (inc n)))))

(split-into [[\# \. \. \#] [\. \. \. \.] [\. \. \. \.] [\# \. \. \#]] 2)
(reconstruct (split-into starting-pattern 3))

(defn step [patterns]
  (fn [grid]
  (let [split-n (case (int (mod (count grid) 2))
                   0 2
                   1 3)]
    (->> (split-into grid split-n)
         (map #(map (partial substitute-for-pattern patterns) %))
         (reconstruct)))))
        ;;  (reconstruct)))))

(defn answer [n]
  (let [patterns (->> (utils/read-input "2017/day21.txt")
                      (map parse-pattern)
                      (reduce merge {}))]
    (->> (nth (iterate (step patterns) starting-pattern) n)
         (map string/join)
         (string/join)
         (seq)
         (filter (partial = \#))
         (count)
         )))
        ;;  (map string/join)
        ;;  (reduce string/join))))

(answer 5)
(time (println "answer part 2" (answer 18)))

(split-into starting-pattern 3)

(let [patterns (->> (utils/read-input "2017/day21-example.txt")
                    (map parse-pattern)
                    (reduce merge {}))]
  (reconstruct  (list (list (list (substitute-for-pattern patterns  (first (first (split-into starting-pattern 3)))))))))

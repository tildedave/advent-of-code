(ns advent2021.day8
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def number-map
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(def number-map-inverted (set/map-invert number-map))

;; so for each character we give a signal for which character
;; it could be.

(def example-line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")

(defn parse-line [str]
  (->> (.split str "\\|")
       (map string/trim)
       (map #(.split % " "))
       (map #(map set %))))

(parse-line example-line)

(use 'clojure.tools.trace)

(defn answer-part1 [lines]
  (->> lines
       (map parse-line)
       (map second)
    ;;    (trace)
       (map #(map count %))
    ;;    (trace)
       (map (fn [l] (filter #(contains? #{2 3 4 7} %) l)))
    ;;    (trace)
       (map count)
    ;;    (trace)
       (reduce +)))

(answer-part1 (list example-line))

(answer-part1 (utils/read-resource-lines "input/day8-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day8.txt"))

;; for part 2, I suppose we can just use brute force and eliminate
;; possibilities.

;; \a -> (\a to \f)
;; etc
;; this is really just any permutation of \a \b \c \d \e \f \g (there are 5040
;; of them)

(defn perm-consistent? [signal-map digit-list]
  (every?
   #(some (fn [s] (= s %)) digit-list)
   (map set (map #(map signal-map %) (vals number-map)))))

;; if a perm IS consistent, we can decode the value.

(def example-map {\a \d \b \e \c \a \d \f \e \g \f \b \g \c})

(map set (map #(map example-map %) (vals number-map)))

(first (parse-line example-line))
(perm-consistent? example-map (first (parse-line example-line)))

(def signal-list '(\a \b \c \d \e \f \g))
(def perm (nth
           (for [p (combo/permutations signal-list)]
             (map vector p signal-list))
           14))
(into {} perm)
(vals number-map)
(map #(map (into {} perm) %) (vals number-map))

(defn decode-digits [[digit-list result]]
  (let [perms (combo/permutations signal-list)]
    (loop [perms perms]
      (if (empty? perms)
        (throw (Exception. "did not find consistent mapping"))
        (let [p (first perms)
              signal-mapping (into {} (map vector signal-list p))]
          (if (perm-consistent? signal-mapping digit-list)
            ;;
            (->> result
                 (map #(map (set/map-invert signal-mapping) %))
                 (map set)
                 (map number-map-inverted)
                 (reduce (fn [acc num] (+ (* acc 10) num))))
            ;; (map (map (map/invert-map signal-mapping) %) result
            (recur (rest perms))))))))

(defn answer-part2 [lines]
  (->> lines
       (map parse-line)
       (map decode-digits)
       (reduce +)))

(answer-part2 (utils/read-resource-lines "input/day8.txt"))

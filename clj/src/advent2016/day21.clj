(ns advent2016.day21
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(def swap-position-re #"^swap position (\d) with position (\d)$")
(def letter-re #"^swap letter (\w) with letter (\w)$")
(def reverse-re #"^reverse positions (\d) through (\d)$")
(def move-position-re #"^move position (\d) to position (\d)$")
(def rotate-n-re #"^rotate (left|right) (\d) steps?$")
(def rotate-letter-re #"^rotate based on position of letter (\w)$")

(def all-res [swap-position-re letter-re reverse-re rotate-n-re rotate-letter-re move-position-re])

(defn parse-instruction [line]
  (if-let [matching-re (first (remove nil? (filter #(re-matches % line) all-res)))]
    (let [[_ x y] (map utils/try-parse-int (re-matches matching-re line))]
      (condp = matching-re
        swap-position-re [:swap-position x y]
        letter-re [:swap-letter (first x) (first y)]
        reverse-re [:reverse-positions x y]
        rotate-n-re [:rotate-n x y]
        move-position-re [:move-position x y]
        rotate-letter-re [:rotate-letter (first x)]))
    (throw (Exception. (format "could not parse %s" line)))))

(->> (utils/read-input "2016/day21-example.txt")
     (map parse-instruction))

(defn make-passwd-map [^String passwd]
  (into {} (map-indexed vector (seq passwd))))

(defn to-passwd [passwd-map]
  (->> passwd-map (sort-by first) (map second) (string/join)))

;; it is easiest to deal with this a char vector I think
;; actually, do we want to just assume this is an associative map?
(defn perform-instruction [passwd-map [instr x y]]
  (case instr
    :swap-position (-> passwd-map
                       (assoc y (get passwd-map x))
                       (assoc x (get passwd-map y)))
    :swap-letter (let [x-pos (first (first (filter (fn [[_ ch]] (= ch x)) passwd-map)))
                       y-pos (first (first (filter (fn [[_ ch]] (= ch y)) passwd-map)))]
                   (-> passwd-map
                       (assoc x-pos y)
                       (assoc y-pos x)))
    :reverse-positions (let [offset (- y x)]
                         (->> passwd-map
                              (map (fn [[n ch]]
                                     (if (<= x n y)
                                       [(- y (- n x)) ch]
                                       [n ch])))
                              (into {})
                              ))
    :rotate-n (let [offset (* (case x "left" -1 1) y)]
                (->> passwd-map
                     (map (fn [[n ch]]
                            [(mod (+ n offset) (count passwd-map)) ch]))
                     (into {})))
    ;; move position x to y.
    ;; I think it is going to be easier to convert the map to a string
    ;; and to the calculations that way.
    ;; this will be more costly but my head will hurt less.
    ;; the passwords are very small anyways.
    ;; (for now.)
    :move-position (let [^String passwd (to-passwd passwd-map)
                         passwd-without-x (str (subs passwd 0 x) (subs passwd (inc x)))]
                        (make-passwd-map
                         (str
                          (subs passwd-without-x 0 y)
                          (str (.charAt passwd x))
                          (subs passwd-without-x y))))
    :rotate-letter (let [x-pos (first (first (filter (fn [[_ ch]] (= ch x)) passwd-map)))
                         offset (+ x-pos 1 (if (>= x-pos 4) 1 0))]
                     (->> passwd-map
                          (map (fn [[n ch]]
                                 [(mod (+ n offset) (count passwd-map))
                                  ch]))
                          (into {})))
  ))


(filter (fn [[_ ch]] (= ch \d))
               {2 \a, 3 \b, 4 \d, 0 \e, 1 \c})

(to-passwd
 (perform-instruction
  (make-passwd-map "ebcda")
  (parse-instruction "swap letter d with letter b")))

(to-passwd
 (perform-instruction
  (make-passwd-map "edcba")
  (parse-instruction "reverse positions 0 through 4")))

(to-passwd
 (perform-instruction
  (make-passwd-map "abcde")
  (parse-instruction "rotate left 1 step")))

(to-passwd
 (perform-instruction
  (make-passwd-map "bcdea")
  (parse-instruction "move position 1 to position 4")))

(to-passwd
 (perform-instruction
  (make-passwd-map "bdeac")
  (parse-instruction "move position 3 to position 0")))

(to-passwd
 (perform-instruction
  (make-passwd-map "abdec")
  (parse-instruction "rotate based on position of letter b")))

(to-passwd
 (perform-instruction
  (make-passwd-map "ecabd")
  (parse-instruction "rotate based on position of letter d")))

(parse-instruction "swap position 4 with position 0")

(to-passwd
 (reduce
  perform-instruction
  (make-passwd-map "abcde")
  (->> (utils/read-input "2016/day21-example.txt")
       (map parse-instruction))))

(defn answer-part1 []
  (to-passwd
   (reduce
    perform-instruction
    (make-passwd-map "abcdefgh")
    (->> (utils/read-input "2016/day21.txt")
         (map parse-instruction)))))

(count (combo/permutations [\a \b \c \d \e \f \g \h]))
(defn answer-part2 []
  (let [instructions (->> (utils/read-input "2016/day21.txt")
                          (map parse-instruction))
        scrambled-password (fn [passwd-map]
                             (to-passwd
                              (reduce
                               perform-instruction
                               passwd-map
                               instructions)))]
    (->> (combo/permutations [\a \b \c \d \e \f \g \h])
         (map #(into {} (map-indexed vector %)))
         (filter #(= (scrambled-password %) "fbgdceah"))
         (first)
         (to-passwd))))

(answer-part2)


(ns advent2020.day18
  (:require [utils :as utils]))

;; I could do this by building a parser but I don't wanna.
;; let's do via string manipulation.
(set! *warn-on-reflection* true)

(defn matching-closing-paren [str n]
  (loop [idx n
         num-opens 0]
    (if
     (>= idx (count str))
      (throw (Exception. "did not find matching closing paren"))
      (let [ch (.charAt str idx)
            num-opens (case ch \) (dec num-opens)
                            \( (inc num-opens)
                            num-opens)]
        (if (= num-opens 0) idx
            (recur (inc idx) num-opens))))))

(def operator-re #"\s*(\*|\+) (.*)")

(re-matches operator-re "1 * 2 + (3 + (2 * 6) + 5)")
(re-matches operator-re "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(defn grab-first-expr [^String str]
  ;; return the expression, the operation, and the rest of the string
  ;; if it's an open paren,
  (let [s (case (.charAt str 0) \( 1 0)
        l (case (.charAt str 0)
            \( (matching-closing-paren str 0)
            1)]
    (into [(.substring str s l)]
          (rest (re-matches operator-re (.substring str (inc l)))))))

(defn eval-expr
  [^String expr]
  (loop [acc 0
         op +
         rest-expr expr]
    (cond (= (count rest-expr) 1)
          (op acc (Integer/parseInt rest-expr))
      ;; otherwise grab the first expression, evaluate it, then recurse.
      :else
          (let [[first next-op rest-expr'] (grab-first-expr rest-expr)]
            (if (nil? next-op)
              (op acc (eval-expr first))
              (recur
               (op acc (eval-expr first))
               (case next-op "*" * "+" +)
               rest-expr'))))))

(grab-first-expr "(8 + 6 * 4)")

(eval-expr "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
(eval-expr "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(eval-expr "1 + 2 + 7 * 9")
(eval-expr "2")

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map eval-expr)
       (reduce +)))

(answer-part1 "day18.txt")

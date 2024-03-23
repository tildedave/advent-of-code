(ns advent2020.day18
  (:require [utils :as utils]
            [clojure.string :as string]))

;; I could do this by building a parser but I don't wanna.
;; let's do via string manipulation.
(set! *warn-on-reflection* true)

(defn matching-paren
  ([str n] (matching-paren str n 1))
  ([str n dn]
   (loop [idx n num-opens 0]
     (if
      (or (< idx 0) (>= idx (count str)))
       (throw (Exception. "did not find matching closing paren"))
       (let [ch (.charAt str idx)
             num-opens (case ch \) (dec num-opens)
                             \( (inc num-opens)
                             num-opens)]
         (if (= num-opens 0) idx
             (recur (+ idx dn) num-opens)))))))

(def operator-re #"\s*(\*|\+) (.*)")

(re-matches operator-re "1 * 2 + (3 + (2 * 6) + 5)")
(re-matches operator-re "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")

(defn grab-first-expr [^String str]
  ;; return the expression, the operation, and the rest of the string
  ;; if it's an open paren,
  (let [s (case (.charAt str 0) \( 1 0)
        l (case (.charAt str 0)
            \( (matching-paren str 0)
            1)]
    (into [(.substring str s l)]
          (rest (re-matches operator-re (.substring str (inc l)))))))

(defn eval-expr-p1
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
              (op acc (eval-expr-p1 first))
              (recur
               (op acc (eval-expr-p1 first))
               (case next-op "*" * "+" +)
               rest-expr'))))))

(grab-first-expr "(8 + 6 * 4)")

(eval-expr-p1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
(eval-expr-p1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(eval-expr-p1 "1 + 2 + 7 * 9")
(eval-expr-p1 "2")

(defn answer-part1 [filename]
  (->> (utils/read-input (format "2020/%s" filename))
       (map eval-expr-p1)
       (reduce +)))

(answer-part1 "day18.txt")

;; for part 2 precedence is a little different.
;; we need to evaluate all + signs first
;; the approach we'll use is to, for each +, add parenthesis around
;; lhs + rhs and then evaluate using the p1 semantics.

(eval-expr-p1 "(((((2 + 4) * 9) * (((6 + 9) * (8 + 6)) + 6)) + 2) + 4) * 2")

(defn parenthify [expr]
  (loop [^String expr expr
         n 0]
    (let [plus-idx (.indexOf expr "+" n)]
      (if (= plus-idx -1)
        expr
        (let [lhs (- plus-idx 2)
              rhs (+ plus-idx 2)
              lhs (if (= (.charAt expr lhs) \)) (matching-paren expr lhs -1) lhs)
              rhs (if (= (.charAt expr rhs) \() (matching-paren expr rhs) rhs)]
          (recur
           (string/join
            [(.substring expr 0 lhs)
             "("
             (.substring expr lhs (inc rhs))
             ")"
             (.substring expr (inc rhs))])
           (+ plus-idx 2)
           ))))))

        ;;   (println (.substring expr lhs (inc rhs))))))))

(eval-expr-p1 (parenthify "1 + (2 * 3) + (4 * (5 + 6))"))
(defn eval-expr-p2 [expr] (eval-expr-p1 (parenthify expr)))
(eval-expr-p2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
(eval-expr-p2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
(eval-expr-p2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")

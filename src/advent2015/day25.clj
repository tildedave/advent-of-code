(ns advent2015.day25
  (:require [utils :as utils]))

(defn next-code [code]
  (mod (* code 252533) 33554393))

(iterate next-code 20151125)

;; so this is inefficient, we can actually go straight to the right place via
;; standard modular exponentiation tricks.

(defn mod-mult [a b p]
  (loop [res 0
         a a
         b b]
    (if (= b 0) res
        (recur
         (if (= (mod b 2) 1) (mod (+ res a) p) res)
         (if (zero? b) a (mod (+ a a) p))
         (bit-shift-right b 1)))))

(defn mod-exp [m n p]
  (loop [pow 1
         m m
         n n]
    (if (= n 0) pow
        (recur
         (if (= (mod n 2) 1) (mod-mult pow m p) pow)
         (mod-mult m m p)
         (bit-shift-right n 1)))))

(defn nth-code [n]
  (mod (* 20151125 (mod-exp 252533 n 33554393)) 33554393))

;; so now we need to get a formula to go from 3,4 to n, the value we can
;; plug into n-th code.

(defn code-num [row column]
  (loop [row row
         column column
         offset 0]
    (if (= column 1)
      (+ (/ (* (dec row) row) 2) 1 offset)
      (recur (inc row) (dec column) (inc offset)))))

(defn answer [filename]
  (let [[_ row col] (re-matches
                     #".*row (\d+), column (\d+)\.$"
                     (first (utils/read-input (format "2015/%s" filename))))]
    (nth-code (dec (code-num (utils/parse-int row) (utils/parse-int col))))))

(answer "day25.txt")

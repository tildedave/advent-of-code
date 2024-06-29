(ns advent2019.day19
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]))

(defn test-square [program x y]
  (a/go
    (let [input (a/chan)
          output (intcode/run-program program input)]
      (>! input x)
      (>! input y)
      (a/close! input)
      (<! output))))

;; part 1
(println
 (let [program (intcode/parse-file "2019/day19.txt")]
   (->>
    (for [x (range 50)
          y (range 50)]
      (<!! (test-square program x y)))
    (filter (partial = 1))
    (count))))

;; so I made part 2 very complicated in golang.
;; this problem is brute forceable.  we will just do this.
;; from https://www.reddit.com/r/adventofcode/comments/ecogl3/comment/fbcxvt1/
(let [program (intcode/parse-file "2019/day19.txt")]
  (loop [x 0 y 100]
    (if (= (<!! (test-square program x y)) 1)
      (if (= (<!! (test-square program (+ x 99) (- y 99))) 1)
        (+ (* x 10000) (- y 99))
        (recur x (inc y)))
      (recur (inc x) y))))

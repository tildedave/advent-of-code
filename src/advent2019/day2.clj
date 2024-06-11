(ns advent2019.day2
  (:require [advent2019.intcode :as intcode]
            [utils :as utils]))

(intcode/step-program (intcode/parse-program "1,0,0,0,99"))

;; answer to part 1
(-> (intcode/parse-file "2019/day2.txt")
    (assoc-in [:program 1] 12)
    (assoc-in [:program 2] 2)
    (intcode/halting-state)
    (get-in [:program 0]))

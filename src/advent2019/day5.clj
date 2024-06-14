(ns advent2019.day5
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [>!! <!!]]))

;; part 1
(let [input (a/chan)
      output (a/chan)]
  (a/go
    (intcode/run-program
     (intcode/parse-file "2019/day5.txt")
     input
     output))
  (>!! input 1)
  (loop []
    (let [res (<!! output)]
      (if (zero? res)
        (recur)
        res))))

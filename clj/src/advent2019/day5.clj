(ns advent2019.day5
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [>!! <!!]]))

;; part 1
(let [output (intcode/run-input-output
              (intcode/parse-file "2019/day5.txt")
              1)]
  (loop []
    (let [res (<!! output)]
      (if (zero? res)
        (recur)
        res))))

;; part 2
(let [output (intcode/run-input-output
              (intcode/parse-file "2019/day5.txt")
              5)]
  (loop []
   (let [res (<!! output)]
     (if (zero? res)
       (recur)
       res))))

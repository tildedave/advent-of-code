(ns advent2018.day1
  (:require [utils :as utils]))

;; part 1
(->> (utils/read-input "2018/day1.txt")
     (map utils/parse-int)
     (reduce +))

;; part 2
(->> (utils/read-input "2018/day1.txt")
     (map utils/parse-int)
     (cycle)
     (reductions +)
     (reduce
      (fn [seen n]
        (if (contains? seen n)
          (reduced n)
          (conj seen n)))
      #{}))

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


12

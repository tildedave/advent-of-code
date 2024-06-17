(ns advent2019.day9
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [>!! <!! <!]]))

(let [input (a/chan)
      output (a/chan)]
 (a/go
   (intcode/run-program (intcode/parse-file "2019/day9.txt")
                      input output))
  (>!! input 1)
  (println (<!! (a/into [] output))))

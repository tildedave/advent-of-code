(ns advent2015.day10
  (:require [utils :as utils]))


(defn step [num-seq]
  (loop [num-seq num-seq
         result-seq []]
    (if-let [n (first num-seq)]
      (let [[eq-n-seq num-seq] (split-with (partial = n) (rest num-seq))]
        (recur num-seq
               (conj result-seq [(inc (count eq-n-seq)) n])))
      (apply concat result-seq))))

(defn step-seq [numbers] (iterate step numbers))

(seq (first (utils/read-input "2015/day10.txt")))

(defn answer [filename num]
  (count (nth (->> (format "2015/%s" filename)
                   (utils/read-input)
                   (first)
                   (seq)
                   (map #(utils/parse-int (str %)))
                   (step-seq)) num)))

(answer "day10.txt" 40)
(answer "day10.txt" 50)

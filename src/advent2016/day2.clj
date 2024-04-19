(ns advent2016.day2
  (:require [utils :as utils]
            [clojure.string :as string]))

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(defn button-move [[x y] dir]
  (let [[nx ny] (case dir
                  \U [x (dec y)]
                  \L [(dec x) y]
                  \R [(inc x) y]
                  \D [x (inc y)])]
    (if (and (<= 0 nx 2) (<= 0 ny 2))
      [nx ny]
      [x y])))

(defn process-instructions [instr-lists]
  (loop [[x y] [1 1]
         current-instr (first instr-lists)
         rest-instrs (rest instr-lists)
         result []]
    (if-let [instr (first current-instr)]
      (recur
       (button-move [x y] instr)
       (rest current-instr)
       rest-instrs
       result)
      (let [next-code (get-in keypad [y x])
            result (conj result next-code)]
        (if (empty? rest-instrs)
          result
          (recur
           [x y]
           (first rest-instrs)
           (rest rest-instrs)
           result))))))

(process-instructions (list (seq "ULL") (seq "RRDDD") (seq "LURDL") (seq "UUUUD")))

(defn answer-part1 []
  (->> (utils/read-input "2016/day2.txt")
       (map seq)
       (process-instructions)
       (string/join "")))

(answer-part1)

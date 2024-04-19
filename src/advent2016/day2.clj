(ns advent2016.day2
  (:require [utils :as utils]
            [clojure.string :as string]))

(def keypad
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(def keypad-p2
  [[nil nil 1 nil nil]
   [nil 2 3 4 nil]
   [5 6 7 8 9]
   [nil "A" "B" "C" nil]
   [nil nil "D" nil nil]])

(defn button-move [keypad [x y] dir]
  (let [[nx ny] (case dir
                  \U [x (dec y)]
                  \L [(dec x) y]
                  \R [(inc x) y]
                  \D [x (inc y)])]
    (if (get-in keypad [ny nx])
      [nx ny]
      [x y])))

(defn process-instructions [keypad instr-lists]
  (loop [[x y] [x y]
         current-instr (first instr-lists)
         rest-instrs (rest instr-lists)
         result []]
    (if-let [instr (first current-instr)]
      (recur
       (button-move keypad [x y] instr)
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

(process-instructions keypad [1 1] (list (seq "ULL") (seq "RRDDD") (seq "LURDL") (seq "UUUUD")))

(process-instructions keypad-p2 [0 2] (list (seq "ULL") (seq "RRDDD") (seq "LURDL") (seq "UUUUD")))

(defn answer-part1 []
  (->> (utils/read-input "2016/day2.txt")
       (map seq)
       (process-instructions keypad [1 1])
       (string/join)
       (utils/parse-int)))

(answer-part1)

(ns advent2022.day21
  (:require [advent2022.utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]))

(def lines (utils/read-resource-lines "input/day21-example.txt"))

(def monkey-num-re #"^(\w+): (\d+)$")
(def monkey-op-re #"(\w+): (\w+) (\+|\*|\-|\/) (\w+)")

(defn parse-monkey [monkey-str]
  (let [num-re (re-matches monkey-num-re monkey-str)
        op-re (re-matches monkey-op-re monkey-str)]
    (cond
      num-re
      (let [[_ monkey-name num-str] num-re]
        {monkey-name {:number (utils/parse-int num-str)}})
      op-re
      (let [[_ monkey-name arg1 op arg2] op-re]
        {monkey-name {:op op :args [arg1 arg2]}})
      :else nil)))

(defn is-number-monkey? [g monkey]
  (contains? (g monkey) :number))

(defn calc [g m]
  (if
   (is-number-monkey? g m) (get-in g [m :number])
   (let [op-nums (map (partial calc g) (get-in g [m :args]))]
     (case (get-in g [m :op])
       "-" (apply - op-nums)
       "*" (apply * op-nums)
       "+" (apply + op-nums)
       "/" (apply / op-nums)))))

(defn part1 [lines]
  (let [g (reduce merge (map parse-monkey lines))]
    (calc g "root")))

(part1 lines)
(part1 (utils/read-resource-lines "input/day21.txt"))

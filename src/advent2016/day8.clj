(ns advent2016.day8
  (:require [clojure.string :as string]
            [utils :as utils]))

(defn parse-line [line]
  (if-let [[_ a b] (re-matches #"^rect (\d+)x(\d+)$" line)]
    [:rect (utils/parse-int a) (utils/parse-int b)]
    (if-let [[_ a b] (re-matches #"^rotate column x=(\d+) by (\d+)$" line)]
      [:rotate-column (utils/parse-int a) (utils/parse-int b)]
      (if-let [[_ a b] (re-matches #"^rotate row y=(\d+) by (\d+)$" line)]
        [:rotate-row (utils/parse-int a) (utils/parse-int b)]
        (throw (Exception. (format "could not parse line %s" line)))))))

(def ^:dynamic y-max 6)
(def ^:dynamic x-max 50)

(defn rect-coords [a b]
  (for [x (range 0 a)
        y (range 0 b)]
    [x y]))

(defn rotate-column [a b]
  (loop [y 0
         result []]
    (if (= y y-max) (reduce merge result)
        (recur (inc y)
               (conj result {[a (mod (+ y b) y-max)] [a y]})))))

(defn rotate-row [a b]
  (loop [x 0
         result []]
    (if (= x x-max) (reduce merge result)
        (recur (inc x)
               (conj result {[(mod (+ x b) x-max) a] [x a]})))))

(defn apply-operation [state [instr a b]]
  (case instr
    :rect (reduce (fn [state [a b]] (assoc-in state [b a] \#)) state (rect-coords a b))
    (:rotate-row :rotate-column)
    (reduce
     (fn [state [[a b] ch]]
       (assoc-in state [b a] ch))
     state
     (update-vals ((case instr :rotate-row rotate-row
                          :rotate-column rotate-column) a b)
                  (fn [[x y]] (get-in state [y x]))))
    ))

(defn initial-state []
  (vec (repeat y-max (vec (repeat x-max \.)))))

(defn state-to-string [state]
  (string/join "\n"
               (for [row state]
                 (string/join row))))

(binding [y-max 3 x-max 7]
  (println (state-to-string
            (reduce apply-operation (initial-state)
                    [[:rect 3 2]
                     [:rotate-column 1 1]
                     [:rotate-row 0 4]
                     [:rotate-column 1 1]]))))

(defn answer-part1 []
  (->> (utils/read-input "2016/day8.txt")
       (map parse-line)
       (reduce apply-operation (initial-state))
       (state-to-string)
       (seq)
       (filter (partial = \#))
       (count)))

(answer-part1)
;; answer part 2 relies on the print out from the first answer.  ez.

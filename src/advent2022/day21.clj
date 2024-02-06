(ns advent2022.day21
  (:require [advent2022.utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]))

(def example-lines (utils/read-resource-lines "input/day21-example.txt"))

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
       "=" (apply = op-nums)
       "-" (apply - op-nums)
       "*" (apply * op-nums)
       "+" (apply + op-nums)
       "/" (apply / op-nums)))))

(defn part1 [lines]
  (let [g (reduce merge (map parse-monkey lines))]
    (calc g "root")))

(part1 example-lines)
(def input-lines (utils/read-resource-lines "input/day21.txt"))
(part1 input-lines)

(defn try-num [g n]
  (assoc-in g ["humn" :number] n))

(defn part2 [lines]
  (let [g (reduce merge (map parse-monkey lines))
        g (assoc-in g ["root" :op] "=")]
    (->> (for [x (range 0 1000)] (calc (try-num g x) "root"))
         (map-indexed (fn [k v] [k v]))
         (remove #(= (second %) false)))))


(let [g (reduce merge (map parse-monkey input-lines))
      g (assoc-in g ["root" :op] "=")
      [arg1 arg2] (get-in g ["root" :args])]
  (for [x (list 0)]
    [(calc (try-num g x) arg1) (calc (try-num g x) arg2)]))

; denominator seems to be 6125.
; numerator differs by 59778 (downwards) every time.
; so: what is our number at 0?
; 313382588286266688
; we want our numerator to be 119497380318324750



(* 44768941183752384 7)

;; it is not in the first 1000 ;-)
(part2 input-lines)

;; so obviously part2 will not be this easy for real input lines.

(ns advent2015.day3
  (:require [utils :as utils]))

(defn step [[houses-so-far [x y]] ch]
  (let [[nx ny] (case ch
                  \^ [x (dec y)]
                  \v [x (inc y)]
                  \> [(inc x) y]
                  \< [(dec x) y])]
    [(update houses-so-far [nx ny] (fnil inc 0))
     [nx ny]]))

(defn visit [instructions]
  (reduce
   step
   [{[0 0] 1} [0 0]]
   (seq instructions)))

(defn answer-part1 [instructions]
  (->> (visit instructions)
       (first)
       (keys)
       (count)))

(answer-part1 "^>v<")
(answer-part1 (first (utils/read-input "2015/day3.txt")))

(defn visit-p2 [instructions]
  (reduce
   (fn [state ch]
     (let [{:keys [houses santa robo-santa turn]} state
           pos-key (if (= (mod turn 2) 0) :santa :robo-santa)
           [houses pos] (step [houses (state pos-key)] ch)]
         (assoc state
                :houses houses
                pos-key pos
                :turn (inc turn))))
   {:houses {[0 0] 2}
    :santa [0 0]
    :robo-santa [0 0]
    :turn 0}
   (seq instructions)))


(defn answer-part2 [instructions]
  (->> (visit-p2 instructions)
       (:houses)
       (keys)
       (count)))

(answer-part2 (first (utils/read-input "2015/day3.txt")))

(ns advent2017.day22
  (:require [utils :as utils]
            [grid :as grid]))

(defn turn [turn-direction direction]
  (case turn-direction
    :left (case direction
            :up :left
            :down :right
            :left :down
            :right :up)
    :right (case direction
             :up :right
             :down :left
             :left :up
             :right :down)))

(defn move [[x y] direction]
  (let [[dx dy] (case direction
                   :up [0 -1]
                   :left [-1 0]
                   :right [1 0]
                   :down [0 1])]
       [(+ x dx) (+ y dy)]))

(defn burst [state]
  (let [{:keys [direction coord infected]} state
        direction (turn (if (contains? infected coord) :right :left) direction)
        action (if (contains? infected coord)
                 :clean
                 :infect)]
    (-> state
        (assoc :direction direction)
        (update :infected #(case action
                             :clean (disj % coord)
                             :infect (conj % coord)))
        (assoc :action action)
        (update :coord #(move % direction)))))

(defn initial-state [filename]
  (let [grid (->> (utils/read-input filename)
                  (grid/parse))
        infected (->> (grid/coords grid)
                      (filter #(= (grid/at grid %) \#))
                      (set))
        _ (println (count grid))]
    {:coord [(quot (count grid) 2) (quot (count grid) 2)]
     :direction :up
     :infected infected}))

(quot 25 2)

(->> (iterate burst (initial-state "2017/day22-example.txt"))
     (take 10000)
     (filter #(= (:action %) :infect))
     (count))

(initial-state "2017/day22.txt")
(->> (iterate burst (initial-state "2017/day22.txt"))
     (take 10000)
     (filter #(= (:action %) :infect))
     (count))

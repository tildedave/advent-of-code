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

(defn reverse-direction [direction]
  (->> direction
       (turn :right)
       (turn :right)))

(defn move [[x y] direction]
  (let [[dx dy] (case direction
                   :up [0 -1]
                   :left [-1 0]
                   :right [1 0]
                   :down [0 1])]
       [(+ x dx) (+ y dy)]))

(defn burst [state]
  (let [{:keys [direction coord status]} state
        action (case (get status coord)
                 :infected :clean
                 :infect)
        direction (turn (case (get status coord)
                          :infected :right
                          :left) direction)
        ]
    (-> state
        (assoc :direction direction)
        (update :status #(case action
                             :clean (dissoc % coord)
                             :infect (assoc % coord :infected)))
        (assoc :action action)
        (update :coord #(move % direction)))))

(defn initial-state [filename]
  (let [grid (->> (utils/read-input filename)
                  (grid/parse))
        status (->> (grid/coords grid)
                    (filter #(= (grid/at grid %) \#))
                    (map #(hash-map % :infected))
                    (reduce merge))]
    {:coord [(quot (count grid) 2) (quot (count grid) 2)]
     :direction :up
     :status status}))

(initial-state "2017/day22.txt")

(->> (iterate burst (initial-state "2017/day22-example.txt"))
     (take 10001)
     (filter #(= (:action %) :infect))
     (count))

(initial-state "2017/day22.txt")
(->> (iterate burst (initial-state "2017/day22.txt"))
     (take 10001)
     (filter #(= (:action %) :infect))
     (count))

(defn burst-part2 [state]
  (let [{:keys [direction coord status]} state
        action (case (get status coord :clean)
                 :clean :weaken
                 :weakened :infect
                 :infected :flag
                 :flagged :clean)
        direction (case (get status coord :clean)
                          :clean (turn :left direction)
                          :weakened direction
                    :infected (turn :right direction)
                    :flagged (reverse-direction direction))]
    (-> state
        (assoc :direction direction)
        (update :status #(case action
                           :clean (dissoc % coord)
                           :flag (assoc % coord :flagged)
                           :weaken (assoc % coord :weakened)
                           :infect (assoc % coord :infected)))
        (assoc :action action)
        (update :coord #(move % direction)))))

(->> (iterate burst-part2 (initial-state "2017/day22.txt"))
     (take 10000001)
     (filter #(= (:action %) :infect))
     (count))

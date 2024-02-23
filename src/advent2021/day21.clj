(ns advent2021.day21
  (:require [advent2021.utils :as utils]))


(def player-re #"^Player (\d+) starting position: (\d+)")

(defn parse-input [filename]
  (let [positions (->> (utils/read-input filename)
       (map (partial re-matches player-re))
       (map rest)
       (map #(mapv utils/parse-int %))
       (into {}))]
    {:positions positions :scores {1 0 2 0}}))

(def deterministic-die (cycle (range 1 101)))

(defn advance-space [current-space increment]
  (-> current-space
      (dec)
      (+ increment)
      (mod 10)
      (inc)))

(defn step [[state player die n]]
  (let [roll-total (->> die (take 3) (reduce +))
        next-space (advance-space roll-total (get-in state [:positions player]))]
    [(-> state
         (assoc-in [:positions player] next-space)
         (update-in [:scores player] (partial + next-space)))
     (case player 1 2 2 1)
     (drop 3 die)
     (+ n 3)]))

(defn is-over? [[state player die n]]
  (let [other-player (case player 1 2 2 1)]
    (>= (get-in state [:scores other-player]) 1000)))

(defn game-seq [filename]
  (iterate step [(parse-input filename) 1 deterministic-die 0]))

(map first (game-seq "day21-example.txt"))

(nth (game-seq "day21-example.txt") 2)

(defn final-score [[state player die n]]
  (* (get-in state [:scores player]) n))

(->> (game-seq "day21-example.txt")
     (reduce #(if (is-over? %1) (reduced %1) %2))
     (final-score))



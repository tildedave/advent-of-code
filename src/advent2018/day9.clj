(ns advent2018.day9
  (:require [utils :as utils]))

(defn parse-input [line]
  (->> line
       (re-matches #"(\d+) players; last marble is worth (\d+) points")
       (rest)
       (map utils/parse-int)))

(->> (utils/read-input "2018/day9.txt")
     (first)
     (parse-input))

(subvec  [1 2 5 6] 1 5)

(defn step [state]
  (let [{:keys [current-player
                current-marble-idx
                current-marble
                circle
                num-players]} state
        next-player (mod (inc current-player) num-players)]
    (if
     (zero? (mod current-marble 23))
      (let [remove-marble-idx (mod (- current-marble-idx 7) (count circle))
            remove-marble (get circle remove-marble-idx)]
        (-> state
            (assoc :current-player next-player)
            (update-in [:scores current-player] (fnil (partial + current-marble remove-marble) 0))
            (update :circle #(into (subvec % 0 remove-marble-idx)
                                   (subvec % (inc remove-marble-idx) (count circle))))
            (update :current-marble inc)
            (assoc :current-marble-idx remove-marble-idx)))
      (let [current-marble-idx (mod (+ current-marble-idx 2) (count circle))]
        (-> state
            (assoc :current-player next-player)
            (update :current-marble inc)
            (update :circle #(-> (subvec % 0 current-marble-idx)
                                 (into [current-marble])
                                 (into (subvec % current-marble-idx (count circle)))))
            (assoc :current-marble-idx current-marble-idx))))))


(defn answer [num-players last-marble]
  (->> (nth (iterate step {:current-player 0
                           :current-marble-idx 0
                           :current-marble 1
                           :circle [0]
                           :num-players num-players})
            last-marble)
       :scores
       (vals)
       (reduce max)))

(answer 9 25)
(answer 10 1618)
(answer 13 7999)

(->> (utils/read-input "2018/day9.txt")
     (first)
     (parse-input)
     (apply answer))

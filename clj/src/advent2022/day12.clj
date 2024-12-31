(ns advent2022.day12
       (:require [utils :as utils]
                 [clojure.string :as string]
                 [clojure.data.priority-map :refer [priority-map]]))

(peek (priority-map "abc" 3 "def" 2))

(def lines (utils/read-resource-lines "input/day12.txt"))
(def grid (mapv identity (string/join "" lines)))

(def num-rows (count lines))
(def num-columns (count (first lines)))
(defn to-idx [[x y]] (+ x (* y num-columns)))
(defn to-xy [idx] [(mod idx num-columns) (quot idx num-columns)])

(defn elevation-at [grid [x y]]
  (let [ch (nth grid (to-idx [x y]))]
    (case ch
      \S 0
      \E 25
      (- (int ch) 97))))

(defn can-move [grid [x y] direction]
  (case direction
    :up (> y 0)
    :down (< y (dec num-rows))
    :left (> x 0)
    :right (< x (dec num-columns))))

(def all-directions (list :up :down :left :right))

(defn move [[x y] direction]
  (case direction
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn get-neighbors [grid [x y]]
  (let [current-elevation (elevation-at grid [x y])
        valid-dirs (filter (partial can-move grid [x y]) all-directions)
        neighbor-positions (map (partial move [x y]) valid-dirs)]
    (filter #(<= (dec (elevation-at grid %)) current-elevation)  neighbor-positions)))

(def adjacency-list
  (reduce
   (fn [acc idx]
     (assoc
      acc
      idx
      (set (map to-idx (get-neighbors grid (to-xy idx))))))
   {}
   (range (* num-columns num-rows))))

(defn update-neighbor-distances [queue distances adjacency-list idx w]
  (->> (adjacency-list idx)
       (reduce
        (fn [[queue distances] n-idx]
          (if (< w (get distances n-idx Integer/MAX_VALUE))
            [(assoc queue n-idx w) (assoc distances n-idx w)]
            [queue distances]))
        [queue distances])))

(defn min-distances [adjacency-list start-idx]
  (loop [[queue distances] [(priority-map start-idx 0) {}]]
    (let [[q d] (peek queue)]
      (cond
        (empty? queue) distances
        :else
        (recur
         (update-neighbor-distances (pop queue) (assoc distances q d) adjacency-list q (inc d)))))))

;; this is our part 1 answer
(time ((min-distances adjacency-list (.indexOf grid \S)) (.indexOf grid \E)))

;; for part 2 we can reverse the adjacency list and search from the end pos
(defn reverse-adjacency-list [adjacency-list]
  (reduce
   (fn [acc k]
     (reduce
      (fn [acc v] (assoc acc v (conj (get acc v #{}) k)))
      acc
      (adjacency-list k))
     )
   {}
   (keys adjacency-list)))

(time
 (let [paths (min-distances (reverse-adjacency-list adjacency-list) (.indexOf grid \E))]
   (->> (range (* num-columns num-rows))
        (map to-xy)
        (filter #(= (elevation-at grid %) 0))
        (map to-idx)
        (map #(get paths % Integer/MAX_VALUE))
        (apply min))))

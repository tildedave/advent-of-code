(ns advent2022.day12
       (:require [advent2022.utils :as utils]
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

(def all-positions
  (vec (for [x (range num-columns)
             y (range num-rows)]
         [x y])))

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

(defn get-min-from-queue [queue distances]
  (->> queue
       (map (fn [q] [q (distances q)]))
       (reduce (fn [acc x]
                 (if (< (second x) (second acc))
                   x
                   acc)))
       (first)))

(defn update-neighbor-distances [queue distances idx w]
  (->> (get-neighbors grid (to-xy idx))
       (map to-idx)
       (reduce
        (fn [[queue distances] n-idx]
          (if (< w (get distances n-idx Integer/MAX_VALUE))
            [(assoc queue n-idx w) (assoc distances n-idx w)]
            [queue distances]))
        [queue distances])))

(priority-map 5 3)

(let [[queue distances] [(priority-map (.indexOf grid \S) 0) {}]]
  [queue distances])

(defn min-path [start-idx]
  (let [end-idx (.indexOf grid \E)]
    (loop [[queue distances] [(priority-map start-idx 0) {}]]
      (let [[q d] (peek queue)]
        (cond
          (= q end-idx) d
          (empty? queue) Integer/MAX_VALUE
          :else
          (recur
           (update-neighbor-distances (pop queue) (assoc distances q d) q (inc d))))))))

;; this is our part 1 answer.  took a while :/
(time (min-path (.indexOf grid \S)))

;; just brute force part 2
;; floyd warshall implementation was too large, ran into OOMs
;; I expect I would have hit this problem with another language too.

;; takes around 5 seconds.  probably could have memoized the distance array /
;; parameterize it by source.
(time
 (->> (range (* num-columns num-rows))
     (map to-xy)
     (filter #(= (elevation-at grid %) 0))
     (map to-idx)
     (map min-path)
     (sort <)
     (first)))

;; I guess an even faster approach would be to apply the
;; algo to a reversed graph.

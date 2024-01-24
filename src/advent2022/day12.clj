(ns advent2022.day12
       (:require [advent2022.utils :as utils]
                 [clojure.string :as string]))

(def lines (utils/read-resource-lines "input/day12-example.txt"))
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
       (filter #(contains? queue %))
       (reduce
        (fn [distances n-idx]
          (if (< w (distances n-idx))
            (assoc distances n-idx w)
            distances))
        distances)))

;; a very painful dijkstra's algorithm
(defn min-path []
  (let [end-idx (.indexOf grid \E)
        initial-distances (vec (repeat (* num-rows num-columns) Integer/MAX_VALUE))
        initial-distances (assoc initial-distances (.indexOf grid \S) 0)]
    (loop [queue (set (range (* num-rows num-columns)))
           distances initial-distances]
      (let [q (get-min-from-queue queue distances)
            d (distances q)
            next-queue (disj queue q)]
        (if (= q end-idx)
          ;; done.  answer is our distance
          d
          (recur
           next-queue
           (update-neighbor-distances queue distances q (inc d))))))))

;; this is our part 1 answer.  took a while :/
;; (time (min-path))

;; part 2 is just floyd warshall.  which will probably be kind of painful too.
all-positions

(def all-positions
  (vec (for [x (range num-columns)
             y (range num-rows)]
         [x y])))

(def all-idx-pairs
  (vec (for [m (range (* num-columns num-rows))
             n (range (* num-columns num-rows))]
         [m n])))

(use 'clojure.tools.trace)

(defn initial-distances []
  (reduce
   (fn [next-distances x]
     (reduce
      (fn [next-distances n]
        (-> next-distances
            (assoc [x n] 1)))
      (assoc next-distances [x x] 0)
      (map to-idx (get-neighbors grid (to-xy x)))))
   (->> all-idx-pairs
       (map (fn [q] [q Integer/MAX_VALUE]))
       (into {}))
   (range (* num-columns num-rows))))

(initial-distances)

((initial-distances) [2 2])

;; now for every node, we find all its neighbors.
;; for each neighbor we put a 1 in the distance placey.
;; this is some insane reduce.

(contains? (set all-idx-pairs) [0 1])
((initial-distances) [4 5])

(defn all-min-paths []
  (reduce
   (fn [distances [k i j]]
     (let [dist-through-k (+ (distances [i k])
                             (distances [k j]))]
       (if (> (distances [i j]) dist-through-k)
         (assoc distances [i j] dist-through-k)
         distances)))
   (initial-distances)
   (for [k (range (* num-columns num-rows))
         i (range (* num-columns num-rows))
         j (range (* num-columns num-rows))]
     [k i j])))

((all-min-paths) (.indexOf grid \S) (.indexOf grid \E))

(let [end-idx (.indexOf grid \E)
      min-distances (all-min-paths)]
  (map
   #(if (= end-idx %) Integer/MAX_VALUE (min-distances [end-idx %]))
   (range (* num-columns num-rows))))

(ns advent2022.day12
       (:require [advent2022.utils :as utils]
                 [clojure.string :as string]
                 [clojure.data.priority-map :refer [priority-map]]))

(peek (priority-map "abc" 3 "def" 2))

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
       (reduce
        (fn [[queue distances] n-idx]
          (if (< w (get distances n-idx Integer/MAX_VALUE))
            [(assoc queue n-idx w) (assoc distances n-idx w)]
            [queue distances]))
        [queue distances])))

(priority-map 5 3)

(let [[queue distances] [(priority-map (.indexOf grid \S) 0) {}]]
  [queue distances])

;; a very painful dijkstra's algorithm
(use 'clojure.tools.trace)
(defn min-path []
  (let [end-idx (.indexOf grid \E)]
    (loop [[queue distances] [(priority-map (.indexOf grid \S) 0) {}]]
      (let [[q d] (peek queue)
            next-queue (pop queue)
            next-distances (assoc distances q d)]
        (if (= q end-idx)
          ;; done.  answer is our distance
          d
          (recur
           (update-neighbor-distances next-queue next-distances q (inc d))))))))

;; this is our part 1 answer.  took a while :/
(min-path)

;; part 2 is just floyd warshall.  which will probably be kind of painful too.

(def all-idxs (range (* num-columns num-rows)))

(def all-idx-pairs
  (for [m all-idxs
             n all-idxs]
         [m n]))

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
   {}
   all-idxs))

;; now for every node, we find all its neighbors.
;; for each neighbor we put a 1 in the distance placey.
;; this is some insane reduce.

(defn all-min-paths []
  (reduce
   (fn [distances [k i j]]
     (let [
           dist-i-j (get distances [i j] Integer/MAX_VALUE)
           dist-i-k (get distances [i k] Integer/MAX_VALUE)
           dist-k-j (get distances [k j] Integer/MAX_VALUE)
           dist-through-k (+ dist-i-k dist-k-j)]
       (if (> (distances dist-i-j) dist-through-k)
         (assoc distances dist-i-j dist-through-k)
         distances)))
   (initial-distances)
   (for [k all-idxs
         i all-idxs
         j all-idxs]
     [k i j])))

;; answer to part 2
(let [end-idx (.indexOf grid \E)
      all-min-distances (all-min-paths)]
  (->> all-idxs
       (filter #(= 0 (elevation-at grid (to-xy %))))
       (map #(all-min-distances [% end-idx]))
       (sort <)
       (first)))

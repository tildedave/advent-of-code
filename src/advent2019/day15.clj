(ns advent2019.day15
  (:require [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [advent2019.intcode :as intcode]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.core.match :refer [match]]
            [graph :as graph]))

;; we need to implement A* or DFS or whatever to find
;; the oxygen system.
;; the challenge is that we don't necessarily know the
;; dimensions of the grid, and the robot will need to backtrack
;; to get to the node in the open set (via commands to robot via input/output).
;; how I did this in golang is that I had access to the "parent" mapping, so I
;; could compute the backtracking myself.
;; I am going to use DFS and do it manually, I suppose.

(def direction-responses {:north 1 :south 2 :west 3 :east 4})
(def reverse-direction {:north :south :south :north :west :east :east :west})

(defn move-in-direction [coords dir]
  (mapv
   +
   coords
   (case dir
     :north [0 -1]
     :south [0 1]
     :east [1 0]
     :west [-1 0])))

(defn direction-between [[x1 y1] [x2 y2]]
  (match [(- x2 x1) (- y2 y1)]
    [-1 0]  :west
    [1 0] :east
    [0 -1] :north
    [0 1] :south
    [_ _] (throw (Exception. (format "tried to move more than one square %s %s" [x1 y1] [x2 y2])))))

(defn path-to-direction-list [coord-list]
  (loop [result []
         current-position (first coord-list)
         coord-list (rest coord-list)]
    (if-let [next-position (first coord-list)]
      (recur
       (conj result (direction-between current-position next-position))
       next-position
       (rest coord-list))
      result)))

(path-to-direction-list '([0 0] [-1 0] [-1 -1] [0 -1]))

(defn path-to [parents node]
  (loop [result []
         node node]
    (if-let [next-node (parents node)]
      (recur (conj result node) next-node)
      ;; kind of funny, I had the same reverse issue in golang
      (vec (rseq (conj result node))))))

(defn path-between
  "We're at node1, we want to get to node2.
   Wind backwards until we find a node on the path to node2 and then
   go forward."
  [parents node1 node2]
  (let [path1 (path-to parents node1)
        path2 (path-to parents node2)
        path2-elements (set path2)]
    (loop [path-backwards path1
           path []]
      (if-let [x (peek path-backwards)]
        (if (contains? path2-elements x)
          (into path (second (split-with (partial not= x) path2)))
          (recur
           (pop path-backwards)
           (conj path x)))
        (into path path2)))))

(defn robot-search []
  ;; this is Dijkstra search
  (let [input (a/chan)
        output (intcode/run-file "2019/day15.txt" input)]
    (loop [current-position [0 0]
           visited #{}
           queue (priority-map [0 0] 0)
           contents {}
           parents {}
           distances {}]
      (if (empty? queue)
        [distances contents]
        (let [[x dist] (peek queue)
              queue (pop queue)
              visited (conj visited x)]
          (loop [directions (->> (path-between parents current-position x)
                                 (path-to-direction-list))]
            (if-let [next-direction (first directions)]
              (do
                (>!! input (direction-responses next-direction))
                (assert (not= (<!! output) 0) "Was not able to backtrack")
                (recur (rest directions)))
              nil))
          ;; now we need to understand which directions we can go in
          (let [[queue contents parents distances]
                (reduce
                 (fn [[queue contents parents distances] direction]
                   (let [v (move-in-direction x direction)]
                     (if (contains? visited v)
                       [queue contents parents distances]
                       (do
                         (>!! input (direction-responses direction))
                         (let [square-contents (<!! output)]
                           ;; we moved our position, so we need to move back
                           ;; also check the distances and so forth.
                           (if (zero? square-contents)
                             ;; we didn't move
                             [queue contents parents distances]
                             (do
                               (>!! input (direction-responses (reverse-direction direction)))
                               (assert (not= (<!! output) 0) "Was not able to backtrack")
                               (if (< (inc dist) (get dist v Integer/MAX_VALUE))
                                 [(assoc queue v (inc dist))
                                  (assoc contents v square-contents)
                                  (assoc parents v x)
                                  (assoc distances v (inc dist))]
                                 [queue contents parents distances]))))))))
                 [queue contents parents distances]
                 '(:north :south :east :west))]
            (recur x visited queue contents parents distances)))))))

(defn oxygen-room [contents]
  (->> contents
       (filter (fn [[_ n]] (= n 2)))
       (first)
       (first)))

(let [[distances contents] (robot-search)]
  (distances (oxygen-room contents)))

;; so part2 is just flood fill on the contents map
;; actually it could be seen as just BFS on the contents map.
;; then max distance.
;; OK, let's try that.

(let [[_ contents] (robot-search)
      [_ distances]
      (graph/breadth-first-search
   (oxygen-room contents)
   (fn [coords]
     (->>
      (for [delta [[-1 0] [1 0] [0 1] [0 -1]]]
       (mapv + coords delta))
      (filter #(contains? contents %)))))]
  (->> distances
       (sort-by second >)
       (first)
       (second)))

;; yes, that worked nicely.

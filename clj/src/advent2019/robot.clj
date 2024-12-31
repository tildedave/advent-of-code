(ns advent2019.robot
  (:require [clojure.core.match :refer [match]]))

;; common between day15 and day25

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

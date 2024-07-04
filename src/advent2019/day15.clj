(ns advent2019.day15
  (:require [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [advent2019.intcode :as intcode]
            [clojure.data.priority-map :refer [priority-map]]
            [advent2019.robot :as robot]
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
          (loop [directions (->> (robot/path-between parents current-position x)
                                 (robot/path-to-direction-list))]
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
                   (let [v (robot/move-in-direction x direction)]
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

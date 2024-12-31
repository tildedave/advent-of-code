(ns advent2019.day25
  (:require [advent2019.intcode :as intcode :refer [send-string!]]
            [advent2019.robot :as robot :refer [path-between]]
            [clojure.core.async :as a :refer [go-loop <!! >! <! >!!]]
            [clojure.string :as string]
            [clojure.set :as set]
            [utils :as utils]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.math.combinatorics :as combo]))

;; for my golang solution, I just made it so I could manually play the game,
;; and I played through the game myself.
;; this time, I'm going to program an autonomous agent.  :-)


(defn take-until-command! [chan]
  (go-loop [lines []]
    (let [[next-line done?]
          (loop [result []]
            (let [ch (<! chan)]
              (cond
                (nil? ch) [(string/join (map char result)) true]
                (= ch 10) [(string/join (map char result)) false]
                :else (recur (conj result ch)))))]
      (cond
        done? (conj lines next-line)
        (= next-line "Command?") lines
        (= next-line "") (recur lines)
        :else (recur (conj lines next-line))))))

;; (let [input (a/chan)
;;       output (intcode/run-file "2019/day25.txt" input)]
;;   (println (second (utils/split-by "Doors here lead:" (<!! (take-until-command! output))))))

(let [death-items #{"giant electromagnet"
                    "molten lava"
                    "infinite loop"
                    "escape pod"
                    "photons"}]
  (defn items-to-take [description-lines]
    (->> description-lines
         (utils/split-by "Items here:")
         (second)
         (map (fn [^String s]
                (if (.startsWith s "- ")
                  (.substring s 2)
                  nil)))
         (remove nil?)
         (remove (partial contains? death-items)))))

(defn neighboring-directions [current-room description-lines]
  (let [result
        (->> description-lines
             (map (fn [s] (case s
                            "- north" :north
                            "- south" :south
                            "- east" :east
                            "- west" :west
                            nil)))
             (remove nil?)
             (set))]
    (if (= current-room "security-checkpoint")
      (disj result :east)
      result)))

(sort-by (partial utils/manhattan-distance [0 0])
         #{[0 -1] [0 1]})

(defn walk-to! [input output parents direction-between current dest-position]
  (println "walking from" current "to" dest-position)
  (let [room-path (path-between parents current dest-position)
        direction-path (loop [curr (first room-path)
                              rest-path (rest room-path)
                              result []]
                         (if-let [next (first rest-path)]
                           (recur
                            next
                            (rest rest-path)
                            (conj result (direction-between [curr next])))
                           result))
        _ (println "full path" room-path)
        _ (println "we will walk" direction-path)]
    (loop [path-to-execute direction-path]
      (if-let [x (first path-to-execute)]
        (do
          (<!! (send-string! input (name x)))
          (if (empty? (rest path-to-execute))
            nil
            (let [walking-lines (<!! (take-until-command! output))]
              (if (= walking-lines ["You can't go that way."])
                (throw (Exception.
                        (format
                         "Error in path finding.  Going from %s to %s, path was %s"
                         current
                         dest-position
                         (string/join " " room-path))))
                (println (string/join "\n" walking-lines)))))
          (recur (rest path-to-execute)))
        nil))))

(defn interactive-solve []
  (let [input (a/chan)
        output (intcode/run-file "2019/day25.txt" input)]
    (loop []
      (println (string/join "\n" (<!! (take-until-command! output))))
      (<!! (send-string! input (read-line)))
      (recur))))

(defn room-name [description]
  (->> description
       (filter (fn [^String s] (.startsWith s "==")))
       (first)
       (re-matches #"^== (.*) ==$")
       (second)
       (.toLowerCase)
       (#(.replaceAll % " " "-"))))

(defn reverse-direction [direction]
  (case direction
    :north :south
    :south :north
    :east :west
    :west :east))

(first (shuffle #{1  2  3}))
(doseq [x (combo/subsets (seq #{1 2 3 4}))]
  (println (set x)))

(defn solve-maze []
  (let [input (a/chan)
        output (intcode/run-file "2019/day25.txt" input)]
    (loop
     [;; we will do dijkstra's algorithm.
      ;; the only way to really do this is to know where we're going.
      ;; so, we will step in, observe description, step back, etc.
      ;; use that to create a link?
      visited #{}
      distance {"hull-breach" 0}
      parents {}
      direction-between {}
      queue (priority-map "hull-breach" 0)
      ;; parents is the node before another node that has the minimum distance.
      current-room "hull-breach"
      inventory #{}]
      (if (empty? queue)
        (do
          ;; OK so now we go to the security checkpoint and brute force it.
          (walk-to! input output parents direction-between current-room "security-checkpoint")
          (let [all-possibilities (map set (combo/subsets (seq inventory)))]
            (loop [remaining-possibilities (shuffle all-possibilities)
                   current-inventory (set inventory)]
              (println (count remaining-possibilities) "possibilities remain")
              (<!! (send-string! input "inv"))
              (do
                (let [actual-inventory (<!! (take-until-command! output))
                      computed-inventory (->> actual-inventory
                                              (filter (fn [s] (.startsWith s "- ")))
                                              (map (fn [s] (.substring s 2)))
                                              (set))]
                  (println current-inventory "vs" computed-inventory)
                  (assert (= current-inventory computed-inventory))))
              (let [items-to-have (set (first remaining-possibilities))]
                (println "current inventory" current-inventory "vs" items-to-have)
                (doseq [take-item (seq (set/difference items-to-have current-inventory))]
                  (<!! (send-string! input (format "take %s" take-item)))
                  (<!! (take-until-command! output)))
                (doseq [drop-item (seq (set/difference current-inventory items-to-have))]
                  (<!! (send-string! input (format "drop %s" drop-item)))
                  (<!! (take-until-command! output)))
                (<!! (send-string! input "east"))
                (let [result (string/join "\n" (<!! (take-until-command! output)))
                      _ (println result)]
                  (if-let [match (re-find #"You should be able to get in by typing (\d+) on the keypad" result)]
                    (utils/parse-int (second match))
                    (cond
                      ;; we need to drop stuff to succeed
                      (re-find #"lighter" result)
                      (recur (->> (rest remaining-possibilities)
                                  (remove #(set/subset? items-to-have %)))
                             items-to-have)
                      ;; we need to take stuff to succeed
                      (re-find #"heavier" result)
                      (recur (->> (rest remaining-possibilities)
                                  (remove #(set/subset? % items-to-have)))
                             items-to-have)
                      :else (throw (Exception. (format "could not understand door result: %s" result))))))))))
        (let [[next-room _] (peek queue)
              queue (pop queue)]
          ;; walk from our current room to the next-room.
          ;; we don't want to read the description of the last room.
          ;; this is kind of annoying.
          (if (not= current-room next-room)
            (walk-to! input output parents direction-between current-room next-room)
            nil)
            ;; otherwise, read the direction of the current room.
            (let [current-room next-room
                  visited (conj visited current-room)
                  description (<!! (take-until-command! output))]
              (doseq [item (items-to-take description)]
                (<!! (send-string! input (format "take %s" item)))
                (println "result of taking item" (<!! (take-until-command! output))))
              ;; next we go in all possible directions, then go backwards.
              (let [[distance parents direction-between queue]
                    (reduce
                     (fn [[distance parents direction-between queue] direction]
                       ;; walk in direction,
                       ;; get the name of the room,
                       ;; walk back in the reverse direction
                       ;; potentially update the queue if this is the fastest
                       ;; way to this room.
                       (<!! (send-string! input (name direction)))
                       (let [neighbor-desc (<!! (take-until-command! output))
                             neighbor (room-name neighbor-desc)]
                         (<!! (send-string! input (name (reverse-direction direction))))
                         (assert (room-name (<!! (take-until-command! output))))
                         ;; so now do the dijkstra thing
                         (if (contains? visited neighbor)
                           [distance parents direction-between queue]
                           (let [alt (inc (distance current-room))]
                             (if (< alt (get distance neighbor Integer/MAX_VALUE))
                               ;; this is our present path
                               [(assoc distance neighbor alt)
                                ;; must include the direction here as otherwise
                                ;; we can't reconstruct the path.
                                (assoc parents neighbor current-room)
                                (-> direction-between
                                    (assoc [current-room neighbor] direction)
                                    (assoc [neighbor current-room] (reverse-direction direction)))
                                (assoc queue neighbor alt)]
                               [distance parents direction-between queue])))))
                     [distance parents direction-between queue]
                     (neighboring-directions current-room description))]
                (recur
                 visited
                 distance
                 parents
                 direction-between
                 queue
                 current-room
                 (set/union inventory (items-to-take description))))))))))

(println "maze result is" (solve-maze))

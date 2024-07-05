(ns advent2019.day25
  (:require [advent2019.intcode :as intcode :refer [read-until-newline! send-string!]]
            [advent2019.robot :as robot :refer [path-between path-to path-to-direction-list]]
            [clojure.core.async :as a :refer [go-loop <!! >! <! >!!]]
            [clojure.string :as string]
            [clojure.set :as set]
            [utils :as utils]
            [graph :as graph]
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
      ;; other stuff that is useful
      descriptions {}
      inventory #{}]
      (if (empty? queue)
        (do
          ;; OK so now we go to the security checkpoint and brute force it.
          (walk-to! input output parents direction-between current-room "security-checkpoint")
          ;; now we brute force it.
          (doseq [items-to-have (combo/subsets (seq inventory))]
            (let [_ (println "items to have" items-to-have)
                  items-to-have (set items-to-have)
                  _ (println "items-to-have" items-to-have)
                  _ (println "inventory" inventory)
                  items-to-drop (set/difference (set inventory) items-to-have)]
              (doseq [take-item (seq items-to-have)]
                (<!! (send-string! input (format "take %s" take-item)))
                (<!! (take-until-command! output)))
              (doseq [drop-item (seq items-to-drop)]
                (<!! (send-string! input (format "drop %s" drop-item)))
                (<!! (take-until-command! output)))
              (println "trying" items-to-have items-to-drop)
              (<!! (send-string! input "east"))
              (println (<!! (take-until-command! output))))))
        (let [[next-room dist] (peek queue)
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
                  description (<!! (take-until-command! output))
                  descriptions (assoc descriptions current-room description)]
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
                 descriptions
                 (set/union inventory (items-to-take description))))))))))



(solve-maze)
;; (interactive-solve)

    ;; we are in a new room.  we read the room.
    ;; we remember its description.
    ;; we mark it as explored
    ;; we see which places it goes.  (and we filter out the death ways eventually)
    ;; we pick up any items that are here.
    ;; we choose a next location, and move there.
    ;; we then recur.
    ;; at a certain point we'll add parameters for the final gate.

;;       coords-to-description (atom {})
;;       current-state (atom {:coords [0 0] :inventory #{}})]
;;   (graph/a*-search
;;    @current-state
;;    (fn [& _] false)
;;    (fn [state]
;;      (let [neighbors (->> (:coords state)
;;           (@coords-to-description)
;;           (utils/split-by "Doors here lead:")
;;           (second)
;;           (map (fn [s] (case s
;;                          "- north" :north
;;                          "- south" :south
;;                          "- east" :east
;;                          "- west" :west
;;                          nil)))
;;           (remove nil?)
;;           (map
;;            (fn [direction]
;;              (-> state
;;                  (update :coords #(mapv + % (case direction
;;                                               :north [0 -1]
;;                                               :south [0 1]
;;                                               :east [-1 0]
;;                                               :west [1 0])))))))]
;;        neighbors))
;;    (fn [& _] 5) ;; heuristic
;;    (fn [& _] 1) ;; distance
;;    identity
;;    (fn [current goal-score came-from]
;;      (let [{:keys [coords]} current
;;            full-path (path-to-direction-list (map :coords (path-between came-from @current-state current)))]
;;        (println "next state" current "current location" @current-state)
;;        (loop [path-to-execute full-path
;;               walking-position (:coords @current-state)]
;;          (if-let [x (first path-to-execute)]
;;            (do
;;             ;;  (println "moving in direction" (name x))
;;              (<!! (send-string! input (name x)))
;;              (let [next-position (robot/move-in-direction walking-position x)
;;                    position-lines (<!! (take-until-command! output))]
;;                (if (= position-lines ["You can't go that way."])
;;                  (throw (Exception.
;;                          (format
;;                           "Error in path finding.  Going from %s to %s, path was %s"
;;                           @current-state
;;                           current
;;                           (string/join " " full-path))))
;;                  nil)
;;             ;;    (println "moved" (name x))
;;                (swap! coords-to-description
;;                       #(assoc % next-position position-lines))
;;                (println (string/join "\n" (@coords-to-description next-position)))
;;                (recur
;;                 (rest path-to-execute)
;;                 next-position)))
;;            (reset! current-state (assoc @current-state :coords walking-position))))
;;        (if (and (= coords [0 0]) (not (contains? @coords-to-description [0 0])))
;;          ;; initial bootstrap, this is dumb
;;          (reset! coords-to-description {[0 0] (<!! (take-until-command! output))})
;;          nil)
;;          false))))

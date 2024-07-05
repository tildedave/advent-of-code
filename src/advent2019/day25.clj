(ns advent2019.day25
  (:require [advent2019.intcode :as intcode :refer [read-until-newline! send-string!]]
            [advent2019.robot :as robot :refer [path-between path-to path-to-direction-list]]
            [clojure.core.async :as a :refer [go-loop <!! >! <! >!!]]
            [clojure.string :as string]
            [clojure.set :as set]
            [utils :as utils]
            [graph :as graph]))

;; for my golang solution, I just made it so I could manually play the game,
;; and I played through the game myself.
;; this time, I'm going to program an autonomous agent.  :-)

(defn take-until-command! [chan]
  (go-loop
   [result []]
    (let [line (<! (read-until-newline! chan))]
      (if (= line "Command?")
        (->> result (remove (partial = "")))
        (recur (conj result line))))))

;; (let [input (a/chan)
;;       output (intcode/run-file "2019/day25.txt" input)]
;;   (println (second (utils/split-by "Doors here lead:" (<!! (take-until-command! output))))))

(defn items-to-take [description-lines]
  (->> description-lines
       (utils/split-by "Items here:")
       (second)
       (map (fn [^String s]
              (if (.startsWith s "- ")
                (.substring s 2)
                nil)))
       (remove nil?)
       (remove #(or (= "giant electromagnet" %) (= "molten lava" %) (= "infinite loop" %)))))

(defn neighboring-rooms [current description-lines]
  (->> description-lines
       (map (fn [s] (case s
                      "- north" :north
                      "- south" :south
                      "- east" :east
                      "- west" :west
                      nil)))
       (remove nil?)
       (map
        (fn [direction]
          (mapv + current (case direction
                                     :north [0 -1]
                                     :south [0 1]
                                     :east [1 0]
                                     :west [-1 0]))))
       (set)))

(sort-by (partial utils/manhattan-distance [0 0])
         #{[0 -1] [0 1]})

(defn walk-to! [input output parents current dest-position]
  (println "walking from" current "to" dest-position)
  (let [full-path (->> (path-between parents current dest-position)
                       (path-to-direction-list))
        _ (println "full path" full-path)]
  (loop [path-to-execute full-path
         walking-position current]
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
                       (string/join " " full-path))))
              (println (string/join "\n" walking-lines)))))
        (recur (rest path-to-execute)
               (robot/move-in-direction walking-position x)))
      nil))))


(defn interactive-solve []
  (let [input (a/chan)
        output (intcode/run-file "2019/day25.txt" input)]
    (loop []
      (println (string/join "\n" (<!! (take-until-command! output))))
      (<!! (send-string! input (read-line)))
      (recur))))

(defn solve-maze []
  (let [input (a/chan)
        output (intcode/run-file "2019/day25.txt" input)]
    (loop
     [state {:position [0 0]
             :descriptions {}
             :parents {}
             :inventory #{}
             :explored #{}
             :unvisited #{}}]
      ;; so the coords aren't usable, we need to rewrite this
      ;; to be more of a graph
      (let [current (:position state)
            description (<!! (take-until-command! output))
            _ (println (string/join "\n" description))
            neighbors (neighboring-rooms current description)
            new-unvisited (set/difference neighbors (:explored state))
            parents (reduce
                     (fn [acc next-position]
                       (assoc acc next-position current))
                     (:parents state)
                     new-unvisited)
        ;;   _ (println "unvisited" )
            unvisited (-> (:unvisited state)
                          (set/union new-unvisited)
                          (disj current))
            _ (println ">> unvisited now" unvisited)]
        (doseq [item (items-to-take description)]
          (<!! (send-string! input (format "take %s" item)))
          (println "result of taking item" (<!! (take-until-command! output))))
        (let [next-position (->> unvisited
                                 (sort-by (partial utils/manhattan-distance current))
                                 (first))]
          (if (nil? next-position)
            (do
              (println unvisited (:explored state) (:inventory state))
              (throw (Exception. "Could not find next position")))
            nil)
          (walk-to! input output parents current next-position)
          (recur (-> state
                     (assoc :position next-position)
                     (update :descriptions #(assoc % current description))
                     (assoc :parents parents)
                     (assoc :unvisited unvisited)
                     (update :inventory #(set/union % (set (items-to-take description))))
                     (update :explored #(conj % current)))))))))

;; (solve-maze)
(interactive-solve)

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

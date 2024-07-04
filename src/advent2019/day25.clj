(ns advent2019.day25
  (:require [advent2019.intcode :as intcode :refer [read-until-newline! send-string!]]
            [advent2019.robot :as robot :refer [path-between path-to path-to-direction-list]]
            [clojure.core.async :as a :refer [go-loop <!! >! <! >!!]]
            [clojure.string :as string]
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

(let [input (a/chan)
      output (intcode/run-file "2019/day25.txt" input)]
  (println (second (utils/split-by "Doors here lead:" (<!! (take-until-command! output))))))

;; I suppose A* is probably fine enough.
;; state representation is # of things in inventory
;; and we don't want to go into death place.
;; I guess A* will be kind of annoying as we have to retrace our steps.
;; probably can't use the generic algo without adding even more arguments
;; to it.
;; we'll use the cutoff function as a way to manage all the global state
;; and navigate to the current place.
;; maybe this will be OK.

(let [input (a/chan)
      output (intcode/run-file "2019/day25.txt" input)
      coords-to-description (atom {})
      current-state (atom {:coords [0 0] :inventory #{}})]
  (graph/a*-search
   @current-state
   (fn [& _] false)
   (fn [state]
     (let [neighbors (->> (:coords state)
          (@coords-to-description)
          (utils/split-by "Doors here lead:")
          (second)
          (map (fn [s] (case s
                         "- north" :north
                         "- south" :south
                         "- east" :east
                         "- west" :west
                         nil)))
          (remove nil?)
          (map
           (fn [direction]
             (-> state
                 (update :coords #(mapv + % (case direction
                                              :north [0 -1]
                                              :south [0 1]
                                              :east [-1 0]
                                              :west [1 0])))))))]
       neighbors))
   (fn [& _] 5) ;; heuristic
   (fn [& _] 1) ;; distance
   identity
   (fn [current goal-score came-from]
     (let [{:keys [coords]} current
           full-path (path-to-direction-list (map :coords (path-between came-from @current-state current)))]
       (println "next state" current "current location" @current-state)
       (loop [path-to-execute full-path
              walking-position (:coords @current-state)]
         (if-let [x (first path-to-execute)]
           (do
            ;;  (println "moving in direction" (name x))
             (<!! (send-string! input (name x)))
             (let [next-position (robot/move-in-direction walking-position x)
                   position-lines (<!! (take-until-command! output))]
               (if (= position-lines ["You can't go that way."])
                 (throw (Exception.
                         (format
                          "Error in path finding.  Going from %s to %s, path was %s"
                          @current-state
                          current
                          (string/join " " full-path))))
                 nil)
            ;;    (println "moved" (name x))
               (swap! coords-to-description
                      #(assoc % next-position position-lines))
               (println (string/join "\n" (@coords-to-description next-position)))
               (recur
                (rest path-to-execute)
                next-position)))
           (reset! current-state (assoc @current-state :coords walking-position))))
       (if (and (= coords [0 0]) (not (contains? @coords-to-description [0 0])))
         ;; initial bootstrap, this is dumb
         (reset! coords-to-description {[0 0] (<!! (take-until-command! output))})
         nil)
         false))))

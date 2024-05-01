(ns advent2016.day22
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [grid :as grid]))

(.split #"\s+" "/dev/grid/node-x0-y0     91T   66T    25T   72%")

(def line-re #"^/dev/grid/node\-x(\d+)\-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T.*$")

(defn parse-line [s]
  (if-let [[m & l] (re-matches line-re s)]
    ((fn [[x y size used avail]]
       {[x y] {:size size
               :used used
               :avail avail}})
     (map utils/parse-int l))
    {}))

(parse-line "/dev/grid/node-x0-y0     91T   66T    25T   72%")

(defn answer-part1 []
  (->>
   (combo/combinations (->> (utils/read-input "2016/day22.txt")
                            (map parse-line)
                            (reduce merge)) 2)
   (filter (fn [[[_ m1] [_ m2]]]
             (or (and (<= (:used m1) (:avail m2)) (> (:used m1) 0))
                 (and (<= (:used m2) (:avail m1)) (> (:used m2) 0)))))
   (count)))

;; well, I guess that we can use A* search for this too.
;; it's only an 864 size grid.
;; the example makes it look like we have the goal data, and
;; then we have the empty space, and we need to move it around.
;; so that reduces the search space.
;; so our "neighbors" function will update the current state,
;; and either move the goal data or the empty space.
;; the goal data can only move into the empty space.
;; OK, so this will end up being just A* search.
;; we probably need to keep the used/avail logic.  the example is easy
;; but ours may not be.
;; there are certain nodes which can't ever be emptied.

(defn initial-state [filesystem]
  (let [empty (->> filesystem
                   (filter #(= (:used (second %)) 0))
                   (first)
                   (first))
        goal (->> filesystem
                  (keys)
                  (filter (fn [[_ y]] (= y 0)))
                  (sort-by first >)
                  (first))]
    {:empty empty
     :goal goal}))

(defn parse-filesystem [filename]
  (->> (utils/read-input filename)
       (map parse-line)
       (reduce merge)))

(defn is-goal? [state]
  (= (:goal state) [0 0]))

(defn adjacent-nodes [filesystem]
  (fn [[x y]]
    (->>
     (for [[dx dy] [[-1 0] [1 0] [0 1] [0 -1]]]
       [(+ x dx) (+ y dy)])
     (filter (fn [spot] (contains? filesystem spot)))
     (set))))

(defn can-copy? [filesystem]
  (let [giant-nodes (->> filesystem
                         (filter #(>= (:size (second %)) 100))
                         (map first)
                         (set))]
  (fn [source dest]
    (not (contains? giant-nodes dest)))))
    ;; true))
    ;; (<= (:used (filesystem source)) (:avail (filesystem dest)))))

(defn copy-into [filesystem]
  (fn [state source dest]
    (->
     (if (= source (:goal state))
       (assoc state :goal dest)
       state)
     (assoc :empty source))))

;;      (assoc))
    ;;  (-> state
    ;;      (assoc :filesytem (-> filesystem
    ;;                            (assoc-in [source :used] 0)
    ;;                            (assoc-in [source :avail] (get-in filesystem [source :size]))
    ;;                            (assoc-in [dest :used] (get-in filesystem [source :used]))
    ;;                            (assoc-in [dest :avail] (- (get-in filesystem [dest :size])
    ;;                                                       (get-in filesystem [dest :used])
    ;;                                                       (get-in filesystem [source :used])))))


(defn neighbors [adjacent-nodes can-copy? copy-into]
  (fn [state]
  ;; if the goal and the empty are next to each other AND the empty node can
  ;; receive the goal node (I assume this will always be possible)
  ;; otherwise we can move the empty spot around.
  ;; it seems like we need both of these.
    (let [{:keys [empty goal]} state]
      (concat
           (cond
             (not (contains? (adjacent-nodes goal) empty)) '()
             (not (can-copy? goal empty)) '()
             :else (list (copy-into state goal empty)))
           (->> (disj (adjacent-nodes empty) goal)
                (filter #(can-copy? % empty))
                (map (fn [node] (copy-into state node empty))))))))

(defn heuristic [state]
  (let [{:keys [empty goal]} state
        [gx gy] goal
        [ex ey] empty]
    (+
     gx
     gy ;; manhattan distance from goal data to start (0)
     (+ (abs (- gx ex)) (+ (abs (- gy ey)))))))

(let [filesystem (parse-filesystem "2016/day22-example.txt")
      neighbors (neighbors (adjacent-nodes filesystem) (can-copy? filesystem) (copy-into filesystem))]
  (-> (initial-state filesystem)
      (neighbors)
      (first)
      (neighbors)))

(let [filesystem (parse-filesystem "2016/day22-example.txt")]
  (grid/a*-search
   (initial-state filesystem)
   is-goal?
   (neighbors (adjacent-nodes filesystem) (can-copy? filesystem) (copy-into filesystem))
   heuristic
   (fn [_ _] 1)))

(defn answer-part2 [filename]
  (let [filesystem (parse-filesystem filename)]
    (first (grid/a*-search
     (initial-state filesystem)
     is-goal?
     (neighbors (adjacent-nodes filesystem) (can-copy? filesystem) (copy-into filesystem))
     heuristic
     (fn [_ _] 1)))))

(answer-part2 "2016/day22-example.txt")
(time (answer-part2 "2016/day22.txt"))

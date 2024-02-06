(ns advent2022.day22
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def example-lines (utils/read-resource-lines "input/day22-example.txt"))
(def input-lines (utils/read-resource-lines "input/day22.txt"))

(defn parse-map-line [str]
  (let [line (->> str
                  (map-indexed (fn [n x] [n x]))
                  (filter #(not= (second %) \space)))]
    {:x (mapv first [(first line) (last line)])
     :column (into {} line)}))

(defn parse-map [lines]
  (let [lines (->> lines
                   (partition-by (partial = ""))
                   (first)
                   (map parse-map-line)
                   (map-indexed (fn [k v] (vector k v))))]
    {:y [0 (dec (count lines))]
     :row (into {} lines)}))

(defn in-bounds? [m [x y]]
  (let [[ymin ymax] (m :y)]
       ;; slightly easier to check for out of bounds and invert the
       ;; conditional.
    (if (or (< y ymin) (> y ymax)) false
        (let [[xmin xmax] (get-in m [:row y :x])]
          (or (< x xmin) (> x xmax))))))

;; we could memoize this I guess.
(defn next-step-coords [m [x y] facing-direction]
  (let [[dx dy] (case facing-direction
                  :up [0 -1]
                  :down [0 1]
                  :left [-1 0]
                  :right [1 0])
        is-horizontal-move (or (= facing-direction :left)
                               (= facing-direction :right))
        [ymin ymax] (m :y)]
    (loop [[x y] [(+ x dx) (+ y dy)]]
      (cond
        ;; theoretically nx is wrong.
        (< y ymin) (recur [x ymax])
        (> y ymax) (recur [x ymin])
        :else
        (let [[xmin xmax] (get-in m [:row y :x])]
          (cond
            (and (< x xmin) is-horizontal-move) (recur [xmax y])
            (and (> x xmax) is-horizontal-move) (recur [xmin y])
            (nil? (get-in m [:row y :column x])) (recur [(+ x dx) (+ y dy)])
            :else [x y]))))))

(defn step [[m [x y] facing-direction] n]
  (loop [[x y] [x y]
         n n]
    (if
     (= n 0) [x y]
     (let [[nx ny] (next-step-coords m [x y] facing-direction)]
       (let [ch (get-in m [:row ny :column nx])]
         (if (= ch \#) [x y]
             (recur [nx ny] (dec n))))))))

(def m (parse-map example-lines))

(step [m [5 4] :up] 1)
(step [m (step [m [5 4] :up] 1) :down] 1)
(step [m [0 6] :left] 1)
(step [m [11 6] :right] 1)

(defn parse-directions [str]
  (->> str
       (partition-by #(or (= % \R) (= % \L)))
       (map string/join)
       (map utils/try-parse-int)))

(parse-directions "10R5L5R10L4R5L5")

(defn start-position [m]
  (let [[ymin] (m :y)
        [xmin] (get-in m [:row ymin :x])]
    [xmin ymin]))

(defn turn [facing-direction turn-direction]
  (case turn-direction
    "L" (case facing-direction
          :up :left
          :left :down
          :down :right
          :right :up)
    "R" (case facing-direction
          :up :right
          :right :down
          :down :left
          :left :up)))

(defn process-instruction
  [[m [x y] facing-direction] instruction]
  (if (number? instruction)
    [m (step [m [x y] facing-direction] instruction) facing-direction]
    [m [x y] (turn facing-direction instruction)]))

(defn facing-value [facing-direction]
  (case facing-direction
    :right 0
    :down 1
    :left 2
    :up 3))

(defn answer-part1 [lines]
  (let
   [m (parse-map lines)
    directions (parse-directions (last lines))
    [_ [x y] final-direction] (reduce
                               process-instruction
                               [m (start-position m) :right]
                               directions)]
    (+
     (* 1000 (inc y))
     (* 4 (inc x))
     (facing-value final-direction))))

(answer-part1 example-lines)
(answer-part1 input-lines)

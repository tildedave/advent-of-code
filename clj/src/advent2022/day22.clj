(ns advent2022.day22
  (:require [utils :as utils]
            [clojure.string :as string]))

(def example-lines (utils/read-resource-lines "input/day22-example.txt"))
(def input-lines (utils/read-resource-lines "input/day22.txt"))

;; this structure may have worked for part 1 but it is very annoying.
;; we will try a more direct structure for part 2.

(defn parse-grid [lines]
  (let [lines (->> lines
                   (partition-by (partial = ""))
                   (first)
                   (map vec)
                   (vec))
        xmax (->> lines
                  (map count)
                  (reduce max))]
    (mapv
     (fn [v]
       (let [padding (repeat (- xmax (count v)) \space)]
         (into [] (concat v padding))))
     lines)))

(defn parse-directions [str]
  (->> str
       (partition-by #(or (= % \R) (= % \L)))
       (map string/join)
       (map utils/try-parse-int)))

(parse-directions "10R5L5R10L4R5L5")

(defn grid-at [grid [x y]]
  (get-in grid [y x]))

(defn faces
  "return the upper left corner of each face of the cube"
  [grid size]
  (loop [y 0
         borders []]
    (if
     (>= y (count grid)) borders
     (let [row (get grid y)
           results (->> (range 0 (count row) size)
                        (map (fn [n] [n (nth row n)]))
                        (filter #(not= (second %) \space))
                        (map #(vector (first %) y)))]
       (recur (+ y size) (concat borders results))))))

;; OK, we have the faces now.
;; it is time to figure out how they connect.
;; two faces connect if - they have the same x coord or y coord (obviously)
;; other face connections is more annoying.
;; we also need to orient the connections, also annoying (there is no global
;; concept of direction)

(defn x-coord-matches? [[x1] [x2]] (= x1 x2))
(defn y-coord-matches? [[_ y1] [_ y2]] (= y1 y2))

(defn face-adjancency [faces size]
  ;; face -> direction [face, facing]
  ;; if you are facing a direction on a face and walk off the edge,
  ;; what face do you end up at, and what is your new facing direction.

  ;; first, compute the easy stuff.
  ;; then, complete the graph using some fuzzy process which I hope works. :-)
  ;; we don't actually need to worry about wrapping off the side for anything.
  (into {} (for [[x y] faces]
             (let [left-fn #(and
                             (x-coord-matches? % [(- x size) y])
                             (y-coord-matches? % [x y]))
                   right-fn #(and
                              (x-coord-matches? % [(+ x size) y])
                              (y-coord-matches? % [x y]))
                   up-fn #(and
                           (x-coord-matches? % [x y])
                           (y-coord-matches? % [x (- y size)]))
                   down-fn #(and
                             (x-coord-matches? % [x y])
                             (y-coord-matches? % [x (+ y size)]))
                   pair-fn (fn [dir] (fn [p] (if (zero? (count p))
                                               nil
                                               (list p dir))))]
               [[x y]
                {:left (mapcat (pair-fn :left) (filter left-fn faces))
                 :right (mapcat (pair-fn :right) (filter right-fn faces))
                 :up (mapcat (pair-fn :up) (filter up-fn faces))
                 :down (mapcat (pair-fn :down) (filter down-fn faces))}]))))

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

(defn reverse-direction [dir]
  (case dir
    :up :down
    :down :up
    :left :right
    :right :left))

(def adj (-> example-lines
             (parse-grid)
             (faces 4)
             (face-adjancency 4)))


(defn can-step-forward? [adjacency face direction]
  (seq (get-in adjacency [face direction])))

;; this is easy but we need to use this to "close" the structure
(defn face-step [adjacency face direction]
  ((adjacency face) direction))


;; if you're on a cube and want to go forward, but can't.
;; you turn RIGHT.  you step forward.  you turn LEFT. you step forward.  you turn RIGHT.
;; this is the sequence we can use to close the cube.
;; it can be reversed, it can be rotated, etc.
;; "1" -> "R1L1R"
;; "1" -> "L1R1L"
;; the instructions may not be able to be carried out, I'd rather
;; just dispatch them and have it give me a new value, or error.
;;

(defn process-face-instruction
  [[adjacency face direction] instruction]
  (cond
    (nil? face) [adjacency face direction]
    (number? instruction) ;; just a step
    (let [adj-info ((adjacency face) direction)]
      (if (empty? adj-info)
        [adjacency nil direction]
        (let [[new-face new-direction] adj-info]
          [adjacency new-face new-direction])))
    (string? instruction)
    [adjacency face (turn direction instruction)]))

;; this works
(reduce
 process-face-instruction
 [adj [4 4] :up]
 (parse-directions "R1L1R"))

(defn process-face-instructions
  [[adj face dir] directions]
  (reduce
   process-face-instruction
   [adj face dir]
   directions))

;; for each face, run the above for all directions, both R1L1R and L1R1L.
;; if you find a new face this was, add it, and repeat.

(defn maybe-add-face [adj [face dir] [new-face new-dir]]
  (if
   (nil? new-face) adj
   (-> adj
       (assoc-in
        [face dir]
        (list new-face new-dir))
       (assoc-in
        [new-face (reverse-direction new-dir)]
        (list face (reverse-direction dir))))))

;; so this works.
;; it's not clear it will get everything on the first pass.
;; it does.  at least for the example.
;; yes, it completes for the actual input too.  cool.
(defn complete-face-adjacency [adj]
  (let [r1-seq (parse-directions "R1L1R")
        l1-seq (parse-directions "L1R1L")]
    (reduce
     (fn [adj face]
       (->> (list :up :down :left :right)
            (remove (partial can-step-forward? adj face))
            (reduce
             (fn [adj dir]
               (let [[_ new-face new-dir] (process-face-instructions [adj face dir] r1-seq)
                     [_ new-face2 new-dir2] (process-face-instructions [adj face dir] l1-seq)]
                 (-> adj
                     (maybe-add-face [face dir] [new-face new-dir])
                     (maybe-add-face [face dir] [new-face2 new-dir2]))))
             adj)))
     adj
     (keys adj))))

;; final thing: given an adjacency, if we step off into the abyss,
;; what is our new coordinate?

(defn coord-face [adj face-size [x y]]
  ;; assumption: the coordinate is within the grid, e.g. there is one face
  ;; for it.
  (->> adj
       (keys)
       (filter (fn [[fx fy]] (and (>= x fx)
                                  (>= y fy)
                                  (< x (+ fx face-size))
                                  (< y (+ fy face-size)))))
       (first)))


;; this is the "start" of the direction.
;; I don't think this works.
(defn direction-anchor
  [[fx fy] face-size dir]
  (case dir
    :up [fx fy]
    :down [(+ fx face-size) (+ fy face-size)]
    :left [fx (+ fy face-size)]
    :right [(+ fx face-size) fy]))

;; let's see where [6 4] goes when we go up (we know we arrive on face [8 0]
;; facing right.  we should arrive at [8 2])
(direction-anchor [4 4] 4 :up)

(defn invert-x [adj face-size [x y]]
  (let [[fx fy] (coord-face adj face-size [x y])
        dxright (- (dec (+ fx face-size)) x)]
    [(+ fx dxright) y]))

(defn invert-y [adj face-size [x y]]
  (let [[fx fy] (coord-face adj face-size [x y])
        dybottom (- (dec (+ fy face-size)) y)]
    [x (+ fy dybottom)]))

(defn rotate-right [adj face-size [x y]]
  (let [[fx fy] (coord-face adj face-size [x y])
        fxright (dec (+ fx face-size))
        fybottom (dec (+ fy face-size))
        dxleft (- x fx)
        dytop (- y fy)
        dxright (- fxright x)
        dybottom (- fybottom y)]
    (cond
      (= x fx) ;; we are on the left side, we move to up
      [(+ fx dybottom) fy]
      (= y fy) ;; we are on the up side, move to right
      [fxright (+ fy dxleft)]
      (= x fxright) ;; we are on the right side, move down
      [(+ fx dybottom) fybottom]
      (= y fybottom) ;; we are on the down side, move left
      [fx (+ fy dxleft)])))

(defn rotate-left [adj face-size [x y]]
  (->> [x y]
       ;; :-)
       (rotate-right adj face-size)
       (rotate-right adj face-size)
       (rotate-right adj face-size)))

(defn rotate-twice [adj face-size [x y]]
  (->> [x y]
       (rotate-right adj face-size)
       (rotate-right adj face-size)))

(defn position-on-face [adj face-size new-face [x y]]
  (let [[fx fy] (coord-face adj face-size [x y])
        [dx dy] [(- x fx) (- y fy)]
        [nfx nfy] new-face]
    [(+ nfx dx) (+ nfy dy)]))


(defn next-step-along-cube
  [adj face-size [x y] dir]
  (let [face (coord-face adj face-size [x y])
        [new-face new-dir] (get-in adj [face dir])
        ;; nx/ny needs to be inverted after this to end up on the other
        ;; end of the cube.
        ;; there is certainly a better way to do this.
        [nx ny] (case [dir new-dir]
                  ([:up :up] [:down :down] [:left :left] [:right :right])
                  [x y]
                  ([:up :right] [:right :down] [:down :left] [:left :up])
                  (rotate-right adj face-size [x y])
                  ([:left :down] [:down :right] [:right :up] [:up :left])
                  (rotate-left adj face-size [x y])
                        ;; now the weirdos
                  ([:up :down] [:down :up] [:left :right] [:right :left])
                  (rotate-twice adj face-size [x y])
                  :else
                  (println "missing case - oh no!" [dir new-dir]))
        [nx ny] (case new-dir
                  (:up :down) (invert-y adj face-size [nx ny])
                  (:right :left) (invert-x adj face-size [nx ny]))]
    [(position-on-face adj face-size new-face [nx ny])
     new-dir]))
;; used for some random examples
(def adj (-> example-lines
             (parse-grid)
             (faces 4)
             (face-adjancency 4)
             (complete-face-adjacency)))
(coord-face adj 4 [11 5])

(next-step-along-cube adj 4 [11 5] :right)


(defn next-step-coords [grid adj face-size [x y] facing-direction]
  ;; naively, we step forward.
  ;; if we've stepped into the abyss, we use the face adjacency map
  ;; to figure it out.
  ;; we will not check for # in this method.  that needs to be done
  ;; in the process-step logic so it can potentially.
  (let [[dx dy] (case facing-direction
                  :up [0 -1]
                  :down [0 1]
                  :left [-1 0]
                  :right [1 0])
        [nx ny] [(+ x dx) (+ y dy)]
        ch (get-in grid [ny nx] \space)]
    (if (not= ch \space) [[nx ny] facing-direction]
        (next-step-along-cube adj face-size [x y] facing-direction))))

(defn step [[grid adj face-size [x y] facing-direction] n]
  (loop [[x y] [x y]
         dir facing-direction
         n n]
    (if
     (= n 0) [[x y] dir]
     (let [[[nx ny] new-dir] (next-step-coords grid adj face-size [x y] dir)]
       (let [ch (get-in grid [ny nx] \#)]
         (if (= ch \#) [[x y] dir]
             (recur [nx ny] new-dir (dec n))))))))

(def grid (parse-grid example-lines))

(get-in grid [2 8])
(step [grid adj 4 [6 4] :up] 1) ;; this should be blocked.
(step [grid adj 4 [10 11] :down] 1)

;; yay, it is.
(get-in grid [8 2])

;; (step [m (step [m [5 4] :up] 1) :down] 1)
;; (step [m [0 6] :left] 1)
;; (step [m [11 6] :right] 1)

(defn start-position [grid face-size]
  (first (faces grid face-size)))

(defn process-instruction
  [[grid adj face-size [x y] facing-direction] instruction]
  (if (number? instruction)
    (let [[[nx ny] new-dir] (step [grid adj face-size [x y] facing-direction] instruction)]
      [grid adj face-size [nx ny] new-dir])
    [grid adj face-size [x y] (turn facing-direction instruction)]))

(defn facing-value [facing-direction]
  (case facing-direction
    :right 0
    :down 1
    :left 2
    :up 3))

(faces (parse-grid input-lines) 4)

(defn answer-part2 [lines]
  (let
   [grid (parse-grid lines)
    face-size 50
    adj (-> grid
            (faces face-size)
            (face-adjancency face-size)
            (complete-face-adjacency))
    directions (parse-directions (last lines))
    start-pos (start-position grid face-size)
    [_ _ _ [x y] final-direction]
    (reduce
     process-instruction
     [grid adj face-size start-pos :right]
     directions)]
    (+
     (* 1000 (inc y))
     (* 4 (inc x))
     (facing-value final-direction))))

(answer-part2 example-lines)
(answer-part2 input-lines)
;; (answer-part1 example-lines)
;; (answer-part1 input-lines)


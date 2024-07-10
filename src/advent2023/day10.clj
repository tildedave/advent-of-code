(ns advent2023.day10
  (:require [grid :as grid]
            [graph :as graph]
            [clojure.set :as set]))

(def connector-map
  {\L [[0 -1] [1 0]]
   \- [[-1 0] [1 0]]
   \| [[0 1] [0 -1]]
   \7 [[-1 0] [0 1]]
   \J [[-1 0] [0 -1]]
   \F [[0 1] [1 0]]})

(defn pipe-neighbors [grid [x y]]
  (for [[dx dy]
        (let [ch (grid/at grid [x y])]
          (if (= ch \S)
            (->> grid/cardinal-directions
                 (filter (fn [[dx dy]]
                           (->> (pipe-neighbors grid [(+ x dx) (+ y dy)])
                                (filter (partial = [x y]))
                                (first)))))
            (get connector-map ch [])))]
    [(+ x dx) (+ y dy)]))

(def example-grid
  '("....."
    ".S-7."
    ".|.|."
    ".L-J."
    "....."))
(pipe-neighbors (grid/parse example-grid) [1 1])

(def example-grid2
  '("..F7."
    ".FJ|."
    "SJ.L7"
    "|F--J"
    "LJ..."))

;; flood fill
;; I guess this is just breadth first search
(defn loop-squares [grid]
  (as-> (grid/coords grid) p
    (filter #(= (grid/at grid %) \S) p)
    (first p)
    (graph/breadth-first-search p (partial pipe-neighbors grid))
    (second p)))

(defn answer-part1 [grid]
  (reduce max (vals (loop-squares grid))))

(pipe-neighbors (grid/parse example-grid2) [1 3])

;; part 1
(loop-squares (grid/parse example-grid2))
(loop-squares (grid/parse-file "2023/day10.txt"))

;; I did part 2 by "blowing up" the grid into 3x3 squares.
;; then, you flood fill starting from the outside.
;; we can just use BFS to flood fill :-)
;; the most annoying part of this is just reconstructing the
;; grid I guess?

(defn start-position [grid]
  (first (filter #(= (grid/at grid %) \S) (grid/coords grid))))

;; ugh S needs special casing.
;; we can fix this by rewriting the grid.
(defn replace-start-with-connector [grid]
  (let [start (start-position grid)
        delta (set (map #(mapv - % start) (pipe-neighbors grid start)))]
    [start (assoc-in grid [(second start) (first start)] (case delta
                                                           #{[1 0] [0 1]} \F
                                                           #{[-1 0] [0 1]} \7
                                                           #{[1 0] [0 -1]} \L
                                                           #{[-1 0] [0 -1]} \J))]))

(replace-start-with-connector (grid/parse example-grid))

(defn to-expanded-coord [[x y]]
  [(inc (* 3 x)) (inc (* 3 y))])

(let [^:const expandor-map {[-1 0] \-
                            [1 0] \-
                            [0 -1] \|
                            [0 1] \|}]
  (defn blow-up [grid [x y]]
  ;; x -> 3x
  ;; y -> 3y
  ;; and place the actual thing in the center.
  ;; can we use the connector map?
    (let [ch (grid/at grid [x y])
          connectors (set (get connector-map ch []))]
      (->>
       (for [dx (range -1 2)
             dy (range -1 2)]
         {(mapv + (to-expanded-coord [x y]) [dx dy])
          (cond
            (= [dx dy] [0 0]) ch
            (contains? connectors [dx dy]) (expandor-map [dx dy])
            :else \.)})
       (into {})))))

(blow-up (grid/parse example-grid) [1 2])

(defn expand-grid [grid]
  (let [expanded-grid (into {} (mapcat (partial blow-up grid) (grid/coords grid)))
        xmax (reduce max (map first (keys expanded-grid)))
        ymax (reduce max (map second (keys expanded-grid)))]
    (->>
     (for [y (range 0 (inc ymax))]
       (->>
        (for [x (range 0 (inc xmax))]
          (expanded-grid [x y]))
        (vec)))
     (vec))))

(expand-grid (second (replace-start-with-connector
                      (grid/parse example-grid))))

;; OK so we flood fill from the expanded-grid

(defn edges [grid]
  (let [[xmax ymax] (grid/bounds grid)]
    (concat
     (for [x (range 0 xmax)] [x 0])
     (for [x (range 0 xmax)] [x (dec ymax)])
     (for [y (range 0 ymax)] [0 y])
     (for [y (range 0 ymax)] [(dec xmax) y]))))


;; actually we should BFS from all the exteriors.
;; then follow
(defn answer-part2 [grid]
  (let [[start-position grid-without-start] (replace-start-with-connector grid)
        expanded-grid (expand-grid grid-without-start)
        loop-squares (set (keys
                           (second
                            (graph/breadth-first-search
                             (to-expanded-coord start-position)
                             (partial pipe-neighbors expanded-grid)))))
        exterior-squares (->> (edges expanded-grid)
                              (filter #(= (grid/at expanded-grid %) \.))
                              (first)
                              ((fn [c]
                                 (keys
                                  (second
                                   (graph/breadth-first-search
                                    c
                                    (fn [node]
                                      (let [all-neighbors (grid/neighbors expanded-grid node grid/cardinal-directions (fn [ch] false))]
                                      (->> all-neighbors
                                           (remove #(contains? loop-squares %))))))))))
                              (set))]
    (as-> (grid/coords expanded-grid) s
        (set s)
        (set/difference s loop-squares)
        (set/difference s exterior-squares)
        (filter (fn [[x y]] (= (mod x 3) (mod y 3) 1)) s)
        (count s))))

(answer-part2 (grid/parse example-grid))
(answer-part2 (grid/parse
               '("..........."
                 ".S-------7."
                 ".|F-----7|."
                 ".||.....||."
                 ".||.....||."
                 ".|L-7.F-J|."
                 ".|..|.|..|."
                 ".L--J.L--J."
                 "...........")))
(answer-part2 (grid/parse
               '(".........."
                 ".S------7."
                 ".|F----7|."
                 ".||....||."
                 ".||....||."
                 ".|L-7F-J|."
                 ".|..||..|."
                 ".L--JL--J."
                 "..........")))
(answer-part2 (grid/parse
               '(".F----7F7F7F7F-7...."
                 ".|F--7||||||||FJ...."
                 ".||.FJ||||||||L7...."
                 "FJL7L7LJLJ||LJ.L-7.."
                 "L--J.L7...LJS7F-7L7."
                 "....F-J..F7FJ|L7L7L7"
                 "....L7.F7||L7|.L7L7|"
                 ".....|FJLJ|FJ|F7|.LJ"
                 "....FJL-7.||.||||..."
                 "....L---J.LJ.LJLJ...")))
;; this one is wrong, it's undercounting the loop tiles.
 (answer-part2
 (grid/parse
  '("FF7FSF7F7F7F7F7F---7"
    "L|LJ||||||||||||F--J"
    "FL-7LJLJ||||||LJL-77"
    "F--JF--7||LJLJ7F7FJ-"
    "L---JF-JLJ.||-FJLJJ7"
    "|F|F-JF---7F7-L7L|7|"
    "|FFJF7L7F-JF7|JL---7"
    "7-L-JL7||F7|L7F-7F7|"
    "L.L7LFJ|||||FJL7||LJ"
    "L7JLJL-JLJLJL--JLJ.L")))

(answer-part2
 (grid/parse-file "2023/day10.txt"))

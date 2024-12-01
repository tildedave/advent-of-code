(ns advent2023.day23
      (:require [grid :as grid]))

(def example-lines
  '("#.#####################"
    "#.......#########...###"
    "#######.#########.#.###"
    "###.....#.>.>.###.#.###"
    "###v#####.#v#.###.#.###"
    "###.>...#.#.#.....#...#"
    "###v###.#.#.#########.#"
    "###...#.#.#.......#...#"
    "#####.#.#.#######.#.###"
    "#.....#.#.#.......#...#"
    "#.#####.#.#.#########v#"
    "#.#...#...#...###...>.#"
    "#.#.#v#######v###.###v#"
    "#...#.>.#...>.>.#.###.#"
    "#####v#.#.###v#.#.###.#"
    "#.....#...#...#.#.#...#"
    "#.#########.###.#.#.###"
    "#...###...#...#...#.###"
    "###.###.#.###v#####v###"
    "#...#...#.#.>.>.#.>.###"
    "#.###.###.#.###.#.#v###"
    "#.....###...###...#...#"
    "#####################.#"))

(defn neighbors [grid [x y]]
  (grid/neighbors grid [x y] grid/cardinal-directions #(= % \#)))

(defn intersections [grid]
  (->> (grid/coords grid)
       (filter
        (fn [[x y]]
          (and (= (grid/at grid [x y]) \.)
               (> (count (neighbors grid [x y])) 2))))))

(def symbol-direction
  {
   \> [1 0]
   \< [-1 0]
   \v [0 1]
   \^ [0 -1]
  })

(defn valid-directions [grid [x y]]
  (->> grid/cardinal-directions
       (remove #(let [[cx cy] (mapv + [x y] %)]
                  (or (grid/out-of-bounds? grid [cx cy])
                      (= (grid/at grid [cx cy]) \#))))
       (filter #(= (mapv + %
                   (mapv (fn [x] (- x))
                         (symbol-direction (grid/at grid (mapv + % [x y])))))
                   [0 0]))))

(valid-directions
 (grid/parse example-lines)
 [5 13])

(intersections (grid/parse example-lines))

;; (first (set '(1 3 5)))

(defn walk [grid [x y] dir]
  (loop [[x y] (mapv + [x y] dir)
         dir dir]
    ;; (println [x y] dir)
    (let [next-coord (mapv + [x y] dir)
          neighbor-coords (set (neighbors grid next-coord))]
      (if (> (count neighbor-coords) 2)
       ;; we're done
        [x y]
        (recur
         next-coord
         (mapv - (first (disj neighbor-coords [x y])) next-coord))))))

(walk (grid/parse example-lines) [3 5] [1 0])

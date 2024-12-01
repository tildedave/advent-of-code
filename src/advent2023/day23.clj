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

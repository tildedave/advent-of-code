(ns advent2023.day23
  (:require [grid :as grid]
            [clojure.set :refer [map-invert]]))

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

(def ^:dynamic part2? false)

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
       (filter #(or part2?
                    (= (mapv + %
                             (mapv (fn [x] (- x))
                                   (symbol-direction (grid/at grid (mapv + % [x y])))))
                       [0 0])))))

(intersections (grid/parse example-lines))

;; (first (set '(1 3 5)))

(defn walk [grid [x y] dir]
  (loop [[x y] [x y]
         dir dir
         n 0]
    ;; (println [x y] dir)
    (let [next-coord (mapv + [x y] dir)
          neighbor-coords (set (neighbors grid next-coord))]
      (if (or (> (count neighbor-coords) 2) (= (count neighbor-coords) 1))
       ;; we're done - why do I need to walk in another direction?
        [(mapv + [x y] dir) (inc n)]
        (recur
         next-coord
         (mapv - (first (disj neighbor-coords [x y])) next-coord)
         (inc n))))))

(walk (grid/parse example-lines) [1 0] [0 1])

(defn connection-graph [grid]
  (let [intersections (intersections grid)
        intersection-set (set intersections)]
    (->> intersections
         (map (fn [node]
                {node
                 (->> (valid-directions grid node)
                      (map (partial walk grid node))
                      (filter (fn [[neighbor _]] (contains? intersection-set neighbor)))
                      (map (fn ([[neighbor cost]] {neighbor cost})))
                      (reduce into {}))}))
         (reduce into {}))))

(connection-graph (grid/parse example-lines))
(binding [part2? true] (connection-graph (grid/parse example-lines)))

(defn start-position [grid]
  (->>
   (grid/coords grid)
   (filter #(= (second %) 0))
   (filter #(= (grid/at grid %) \.))
   (first)))

(defn end-position [grid]
  (let [[_ ymax] (grid/bounds grid)]
    (->>
     (grid/coords grid)
     (filter #(= (second %) (dec ymax)))
     (filter #(= (grid/at grid %) \.))
     (first))))

(start-position (grid/parse example-lines))
(end-position (grid/parse example-lines))

;; so the way I did this for golang was a queue + bitset
;; I suppose the same approach will work here
;; seems like I did longest path for 2016 day 17 using a roughly similar
;; approach.  I didn't do any cutoffs for that one.

(defn find-longest-path
  ([grid] (find-longest-path grid 100000))
  ([grid cutoff]
  (let [[start-intersection start-dist] (walk grid (start-position grid) [0 1])
        [end-intersection end-dist] (walk grid (end-position grid) [0 -1])
        graph (connection-graph grid)
        bitset-idx-to-node (into {} (map-indexed vector (keys graph)))
        node-to-bitset-idx (map-invert bitset-idx-to-node)]
    (loop
     [best-so-far -1
      queue [[start-intersection 0 (bit-set 0 (node-to-bitset-idx start-intersection))]]
      n 0]
      (if (> n cutoff)
        :fail
        (if (empty? queue)
          (+ best-so-far start-dist end-dist)
          (let [[x cost seen] (first queue)
                queue (rest queue)]
          (if (= x end-intersection)
            (recur
             (max best-so-far cost)
             queue
             (inc n))
            (recur
             best-so-far
             (reduce
              (fn [queue [node ncost]]
                (let [node-idx (node-to-bitset-idx node)]
                  (if (bit-test seen node-idx)
                    queue
                    (conj queue [node (+ cost ncost) (bit-set seen node-idx)]))))
              queue
              (graph x))
             (inc n)
             )))))))))

(find-longest-path (grid/parse example-lines))
;; correct
(find-longest-path (grid/parse-file "2023/day23.txt"))


(binding [part2? true] (find-longest-path (grid/parse example-lines)))
;; brute force enumeration works \o/
(binding [part2? true] (find-longest-path (grid/parse-file "2023/day23.txt") Integer/MAX_VALUE))

(ns advent2024.day15
  (:require
   [grid :as grid]
   [utils :as utils]))
grid/out-of-bounds?

(defn moved-squares [grid coords direction]
  ;; assumption: grid coords is @
  ;; (assert (= (grid/at grid coords) \@))
  ;; part2 breaks this assumption; we'll use this function recursively
  (let [ray-direction (case direction
                        :up [0 -1]
                        :right [1 0]
                        :down [0 1]
                        :left [-1 0])]
    ;; seems like not looping would be cooler but w/e
    (loop [curr coords
           affected []]
      (let [next (mapv + curr ray-direction)]
        (if
         (grid/out-of-bounds? grid next) (assert "should have hit boundary")
         (case (grid/at grid next)
           \# []
           \O (recur next (conj affected [curr next]))
           ;; part 2 logic can go right here, woo-hoo
           ;; the logic is roughly the same
           (\[ \])
           (case direction
             ;; going left or right, boring, just expand a bit more
             (:left :right)
             (let [next-next (mapv + next ray-direction)]
               (recur next-next
                      (-> affected
                          (conj [curr next]
                                [next next-next]))))
             ;; here's the rub
             ;; we view the box as an "actor" itself, pushing up.
             ;; both sides of the box need to push.
             ;; we have to also merge result, e.g. pushing a directly aligned
             ;; box
             (:up :down)
             (let [[left-box right-box]
                   (case (grid/at grid next)
                     \] [(mapv + next [-1 0]) next]
                     \[ [(mapv + next [1 0]) next])
                   left-moved (moved-squares grid left-box direction)
                   right-moved (moved-squares grid right-box direction)]
               ;; there's an extra assumption on move order here
               ;; we can probably use a sorter in the consumption to do it for
               ;; us
               ;; so a very cavalier approach here would be
               (cond (empty? left-moved)
                     []
                     (empty? right-moved)
                     []
                     :else
                     (vec (distinct
                      (into (-> affected
                               (conj [curr next])
                               (conj [left-box (mapv + left-box ray-direction)])
                               (conj [right-box (mapv + right-box ray-direction)]))
                           (concat left-moved right-moved)))))))
           \. (conj affected [curr next])))))))

(defn start-location [grid]
  (->> grid (grid/coords) (filter #(= (grid/at grid %) \@)) (first)))

(def example-grid
  '("##########"
    "#..O..O.O#"
    "#......O.#"
    "#.OO..O.O#"
    "#..O@..O.#"
    "#O#..O...#"
    "#O..O..O.#"
    "#.OO.O.OO#"
    "#....O...#"
    "##########"))

(def example
  '("##########"
    "#..O..O.O#"
    "#......O.#"
    "#.OO..O.O#"
    "#..O@..O.#"
    "#O#..O...#"
    "#O..O..O.#"
    "#.OO.O.OO#"
    "#....O...#"
    "##########"
    ""
    "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
    "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
    "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
    "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
    "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
    "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
    ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
    "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
    "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
    "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"))

(let [grid (grid/parse example-grid)]
  (moved-squares grid (start-location grid) :left))

(defn swap-coords [grid from to]
  (assert (= (grid/at grid to) \.))
  (-> grid
      (grid/assoc to (grid/at grid from))
      (grid/assoc from \.)))

;; if moving left, want to process moving the leftmost (lower)
(defn dependency-order [direction [_ t1] [_ t2]]
  (case direction
    :left (compare (first t1) (first t2))
    :right (compare (first t2) (first t1))
    :up (compare (second t1) (second t2))
    :down (compare (second t2) (second t1))))

(defn try-move [[grid curr] direction]
  (let [moved-squares (moved-squares grid curr direction)]
    (if (empty? moved-squares)
      [grid curr]
      ;; otherwise we modify the list
      [(reduce (fn [grid [from to]] (swap-coords grid from to)) grid (sort (partial dependency-order direction) moved-squares))
       (second (first moved-squares))])))

(defn parse-direction [ch]
  (case ch
    \^ :up
    \< :left
    \> :right
    \v :down))

(let [grid (grid/parse example-grid)]
  (try-move [grid (start-location grid)] :left))

(def ^:dynamic part2? false)

(def smaller-example
  '("########"
    "#..O.O.#"
    "##@.O..#"
    "#...O..#"
    "#.#.O..#"
    "#...O..#"
    "#......#"
    "########"
    ""
    "<^^>>>vv<v>>v<<"))

(defn box-coords [grid]
  (->> (grid/coords grid) (filter #(case (grid/at grid %) (\O \[) true false))))

(defn expand-grid [grid]
  (let [[max-x max-y] (grid/bounds grid)]
    (vec
     (for [y (range 0 max-y)]
       (->>
        (reduce into []
                (for [x (range 0 max-x)]
                  (case (grid/at grid [x y])
                    \# [\# \#]
                    \O [\[ \]]
                    \. [\. \.]
                    \@ [\@ \.]))))))))

(defn execute-robot [lines]
  (let [[grid-lines direction-lines] (utils/split-by "" lines)
        grid (grid/parse grid-lines)
        grid (if part2? (expand-grid grid) grid)
        [final-grid _] (reduce try-move
                           [grid (start-location grid)]
                           (->> direction-lines
                                (mapcat (partial map parse-direction))))]
    (->> (box-coords final-grid)
         (map (fn [[x y]] (+ x (* 100 y))))
         (reduce +))))

(execute-robot smaller-example)

(execute-robot example)

(execute-robot (utils/read-input "2024/day15.txt"))

(expand-grid (grid/parse example-grid))

(def double-push-example
  '("#######"
    "#...#.#"
    "#.....#"
    "#..OO@#"
    "#..O..#"
    "#.....#"
    "#######"
    ""
    "<vv<<^^<<^^"))

(defn execute-robot-debug [lines n]
  (let [[grid-lines direction-lines] (utils/split-by "" lines)
        grid (grid/parse grid-lines)
        grid (if part2? (expand-grid grid) grid)
        [final-grid _] (reduce try-move
                               [grid (start-location grid)]
                               (->> direction-lines
                                    (mapcat (partial map parse-direction))
                                    (take n)))]
    final-grid))

(binding [part2? true] (execute-robot-debug double-push-example 11))

(binding [part2? true] (execute-robot example))
(binding [part2? true] (utils/read-input "2024/day15.txt"))

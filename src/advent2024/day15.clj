(ns advent2024.day15
  (:require
   [grid :as grid]
   [utils :as utils]))
grid/out-of-bounds?

(defn moved-squares [grid coords direction]
  ;; assumption: grid coords is @
  (assert (= (grid/at grid coords) \@))
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

(defn try-move [[grid curr] direction]
  (let [moved-squares (moved-squares grid curr direction)]
    (if (empty? moved-squares)
      [grid curr]
      ;; otherwise we modify the list
      [(reduce (fn [grid [from to]] (swap-coords grid from to)) grid (rseq moved-squares))
       (second (first moved-squares))])))

(defn parse-direction [ch]
  (case ch
    \^ :up
    \< :left
    \> :right
    \v :down))

(let [grid (grid/parse example-grid)]
  (try-move [grid (start-location grid)] :left))

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
  (->> (grid/coords grid) (filter #(= (grid/at grid %) \O))))

(defn execute-robot [lines]
  (let [[grid-lines direction-lines] (utils/split-by "" lines)
        grid (grid/parse grid-lines)
        [final-grid _] (reduce try-move
                           [grid (start-location grid)]
                           (->> direction-lines
                                (mapcat (partial map parse-direction))))]
    (->> (box-coords final-grid)
         (map (fn [[x y]] (+ x (* 100 y))))
         (reduce +))))

(execute-robot example)

(execute-robot (utils/read-input "2024/day15.txt"))

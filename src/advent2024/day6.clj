(ns advent2024.day6
  (:require
   [grid :as grid]))

(def example-input
  '("....#....."
    ".........#"
    ".........."
    "..#......."
    ".......#.."
    ".........."
    ".#..^....."
    "........#."
    "#........."
    "......#..."))

(def orientation-to-offset
  {\^ [0 -1]
   \v [0 1]
   \> [1 0]
   \< [-1 0]})

(defn turn-right [ch]
  (case ch
    \^ \>
    \> \v
    \v \<
    \< \^))

(defn starting-position [grid]
  (->> (grid/coords grid)
       (filter #(= \^ (grid/at grid %)))
       (first)))

(defn walk [grid]
  (loop
   [grid grid
    [gx gy] (starting-position grid)
    seen #{}]
    (let [guard-dir (grid/at grid [gx gy])
          next-pos (mapv + [gx gy] (orientation-to-offset (grid/at grid [gx gy])))]
      (cond
        (grid/out-of-bounds? grid next-pos) (conj seen [gx gy])
        (= (grid/at grid next-pos) \#)
        (recur
         (assoc-in grid [gy gx] (turn-right guard-dir))
         [gx gy]
         seen)
        :else (recur
         (-> grid
             (assoc-in [gy gx] \.)
             (assoc-in (into [] (reverse next-pos)) guard-dir))
         next-pos
         (conj seen [gx gy]))))))

(count (walk (grid/parse-file "2024/day6.txt")))

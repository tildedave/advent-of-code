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
        ;;   _ (println [gx gy] guard-dir "I seen!")
        ;;   _ (println seen)
          next-pos (mapv + [gx gy] (orientation-to-offset guard-dir))]
      (cond
        (contains? seen [[gx gy] guard-dir]) (throw (ex-info "infinite loop" {}))
        (grid/out-of-bounds? grid next-pos) (conj seen [[gx gy] guard-dir])
        (= (grid/at grid next-pos) \#)
        (recur
         (assoc-in grid [gy gx] (turn-right guard-dir))
         [gx gy]
         (conj seen [[gx gy] guard-dir]))
        :else (recur
               (-> grid
                   (assoc-in [gy gx] \.)
                   (assoc-in (into [] (reverse next-pos)) guard-dir))
               next-pos
               (conj seen [[gx gy] guard-dir]))))))

;; part 1
(println (count (set (map first (walk (grid/parse-file "2024/day6.txt"))))))

;; (println (walk (assoc-in (grid/parse-file "2024/day6.txt") [11 48] \#)))

;; part 2
(defn count-obstructions [grid]
  (->> (set (map first (walk grid)))
       (filter #(= (grid/at grid %) \.))
       (filter #(try
                  (do
                    (walk (assoc-in grid (into [] (reverse %)) \#))
                    false)
                  (catch clojure.lang.ExceptionInfo e
                    true)))))

(println (count-obstructions (grid/parse example-input)))
(println (count (count-obstructions (grid/parse-file "2024/day6.txt"))))

(ns advent2019.day17
  (:require [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [advent2019.intcode :as intcode]
            [clojure.string :as string]
            [grid :as grid]
            [clojure.core.match :refer [match]]))

(defn robot-grid []
  (let [output (intcode/run-file "2019/day17.txt")]
    (->> (<!! (a/into [] output))
         (partition-by (partial = 10))
         (take-nth 2)
         (map #(map (fn [n]
                      (case n
                        35 \#
                        46 \.
                        94 \^)) %)))))

(def example-grid
  '("..#.........."
   "..#.........."
   "#######...###"
   "#.#...#...#.#"
   "#############"
   "..#...#...#.."
   "..#####...^.."))

(grid/parse example-grid)


(defn scaffold-score [grid]
  (->> (grid/coords grid)
       (filter
        (fn [[x' y']]
          (and
           (= (grid/at grid [x' y']) \#)
           (every?
           (fn [[x y]] (= (grid/at grid [x y]) \#))
           (grid/neighbors grid [x' y'])))))
       (map #(apply * %))
       (reduce +)
       ))

;; part 1
(scaffold-score (grid/parse example-grid))
(scaffold-score (grid/parse (robot-grid)))

;; now we forget all about that.
;; we don't try any solving or whatever.
;; we just walk straight forward and turn when we have to.

(defn start-position [grid]
  (->> (grid/coords grid)
       (filter (fn [c] (= (grid/at grid c) \^)))
       (first)))

(start-position (grid/parse (robot-grid)))

(def example-grid2
  '("#######...#####"
    "#.....#...#...#"
    "#.....#...#...#"
    "......#...#...#"
    "......#...###.#"
    "......#.....#.#"
    "^########...#.#"
    "......#.#...#.#"
    "......#########"
    "........#...#.."
    "....#########.."
    "....#...#......"
    "....#...#......"
    "....#...#......"
    "....#####......"))

(defn move [grid [x y] dir]
  (let [next-pos (mapv + [x y] (case dir :up [0 -1] :down [0 1] :left [-1 0] :right [1 0]))
        ;; _ (println next-pos (grid/in-bounds? grid next-pos) (grid/at grid next-pos))
        ]
    (if (and (grid/in-bounds? grid next-pos)
             (= (grid/at grid next-pos) \#))
      next-pos
      nil)))

(defn apply-turn [turn-direction dir]
  (match [turn-direction dir]
    [:L :up] :left
    [:L :left] :down
    [:L :down] :right
    [:L :right] :up
    [:R :up] :right
    [:R :left] :up
    [:R :down] :left
    [:R :right] :down))

(defn turn [grid [x y] dir]
  ;; so the idea here is that we can't go straight.
  ;; so, we want to know if we turn left / right, can we go straight?
  (->> '(:L :R)
       (filter
        (fn [turn-direction]
          (move grid [x y] (apply-turn turn-direction dir))))
       (first)))

;; we won't be smart.  we're just going to go straight.
(defn path [grid]
  (loop
   [[x y] (start-position grid)
    dir :up
    result []]
    (if-let [[x y] (move grid [x y] dir)]
      (recur [x y] dir (conj result 1))
      (if-let [turn-direction (turn grid [x y] dir)]
        (recur [x y] (apply-turn turn-direction dir) (conj result turn-direction))
        result))))

(defn robot-instructions [grid]
  (->> (path grid)
       (partition-by identity)
       (map (fn [c]
              (case (count c) 1
                    (case (first c) :L \L :R \R)
                    (count c))))
       (string/join ",")))

(robot-instructions (grid/parse (robot-grid)))

;; final thing we need to do is combine it into sub-instructions
;; that seems kinda annoying, I'll do it tomorrow.

(.split #",?R,8,R,8,?"
        "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
        )

(.split (re-pattern (format ",?%s,?" "R,4,L,12,L,8,R,4"))
        "R,4,L,12,L,8,R,4,L,8,R,10,R,10,R,6,R,4,L,12,L,8,R,4,R,4,R,10,L,12,R,4,L,12,L,8,R,4,L,8,R,10,R,10,R,6,R,4,L,12,L,8,R,4,R,4,R,10,L,12,L,8,R,10,R,10,R,6,R,4,R,10,L,12")

;; this approach will work.
;; split based on first prefix.  then for all strings < 20, try splitting on that,
;; if final result as a set is singleton, we're done.

(defn split-instruction [^String s prefix]
  (.split (re-pattern (format ",?%s,?" prefix)) s))

(defn try-split [^String s prefix]
  (let [parts (remove (partial = "") (split-instruction s prefix))
        candidates (set (filter #(<= (count %) 20) parts))]
    (for [candidate candidates]
      (let [remaining (set (mapcat #(split-instruction % candidate) parts))]
        (if (and (= (count remaining) 1)
                 (<= (count (first remaining)) 20))
          [prefix candidate (first remaining)]
          nil)))))

(defn valid-prefixes [^String s]
  ;; all strings starting a 0 and ending < 20 so that the final thing isn't a
  ;; comma
  (->> (range 20 0 -1)
       (map #(.substring s 0 %))
       (remove #(= (last %) \,))))

(defn compress-instructions [^String s]
  (->> (valid-prefixes s)
       (mapcat (partial try-split s))
       (remove nil?)
       (map (fn [[a b c]]
              [(-> s
                   (.replaceAll a "A")
                   (.replaceAll b "B")
                   (.replaceAll c "C"))
               a b c]))
       (filter (fn [[s & _]] (<= (count s) 20)))))

;; OK so we have everything.  we now put it together.

(map int "A,B,A,C,A,B,A,C,B,C")

(string/join (map char [46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 46 35 35 35 35 35 35 35 35 35 35 35 46 46 46 46 46 46]))

(defn answer-part2 []
  (let [[main a b c] (->> (robot-grid)
                          (grid/parse)
                          (robot-instructions)
                          (compress-instructions)
                          (first))
        input (a/chan)
        program (assoc-in (intcode/parse-file "2019/day17.txt")
                          [:program 0] 2)
        output (intcode/run-program program input)]
    (<!! (a/go-loop []
           (let [next (<! (intcode/read-until-newline! output))]
             (if (int? next)
               next
               (do
                 (case next
                 "Main:"
                   (intcode/send-string! input main)
                 "Function A:"
                   (intcode/send-string! input a)
                 "Function B:"
                   (intcode/send-string! input b)
                 "Function C:"
                   (intcode/send-string! input c)
                 "Continuous video feed?"
                   (do
                     (>! input (int \n))
                     (>! input 10))
                 nil)
                 (recur))))))))

(println (answer-part2))

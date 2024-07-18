(ns advent2023.day16
  (:require [grid :as grid]
            [clojure.string :as string]))

(def example-grid
  '(".|...\\...."
    "|.-.\\....."
    ".....|-..."
    "........|."
    ".........."
    ".........\\"
    "..../.\\\\.."
    ".-.-/..|.."
    ".|....-|.\\"
    "..//.|...."))

(defn move [position direction]
  (mapv + position (case direction
                     :up [0 -1]
                     :down [0 1]
                     :left [-1 0]
                     :right [1 0])))

(move [0 0] :right)

(defn visualize-energy [grid visited]
  (let [[xmax ymax] (grid/bounds grid)
        visited-squares (set (map first visited))]
    (string/join "\n"
                 (for [y (range 0 ymax)]
                   (string/join
                    (for [x (range 0 xmax)]
                      (if (contains? visited-squares [x y])
                        \#
                        \.)))))))

(defn navigate-grid [grid start direction]
  (loop [queue [[start direction]]
         visited #{}]
    (if (empty? queue)
      visited
      (let [[position direction] (first queue)
            queue (subvec queue 1)]
        (if (contains? visited [position direction])
          (recur queue visited)
          (if-let [ch (grid/at grid position)]
            (let [visited (conj visited [position direction])]
              (case ch
                \. (recur
                    (conj queue [(move position direction) direction])
                    visited)
                \| (case direction
                     (:left  :right)
                     (recur
                      (-> queue
                          (conj [(move position :up) :up])
                          (conj [(move position :down) :down]))
                      visited)
                     (:up :down)
                     (recur
                      (conj queue [(move position direction) direction])
                      visited))
                \- (case direction
                     (:up :down)
                     (recur
                      (-> queue
                          (conj [(move position :left) :left])
                          (conj [(move position :right) :right]))
                      visited)
                     (:left :right)
                     (recur (conj queue [(move position direction) direction])
                            visited))
                \\ (let [new-direction (case direction
                                         :left :up
                                         :up :left
                                         :right :down
                                         :down :right)]
                     (recur (conj queue [(move position new-direction) new-direction])
                            visited))
                \/ (let [new-direction (case direction
                                         :up :right
                                         :right :up
                                         :left :down
                                         :down :left)]
                     (recur (conj queue [(move position new-direction) new-direction])
                            visited))))
            (recur queue visited)))))))

(count (set (map first (navigate-grid (grid/parse example-grid) [0 0] :right))))
;; part 1

(defn num-energized [grid start direction]
  (->> (navigate-grid grid start direction)
       (map first)
       (set)
       (count)))

(num-energized (grid/parse-file "2023/day16.txt") [0 0] :right)

(defn border [grid direction]
  (let [[xmax ymax] (grid/bounds grid)]
    (case direction
      :left (for [y (range 0 ymax)] [(dec xmax) y])
      :right (for [y (range 0 ymax)] [0 y])
      :up (for [x (range 0 xmax)] [x (dec ymax)])
      :down (for [x (range 0 xmax)] [x 0]))))

(defn answer-part2 [grid]
  (->> '(:left :right :up :down)
       (map (fn [dir] (map #(num-energized grid % dir) (border grid dir))))
       (map #(reduce max %))
       (reduce max)))

(answer-part2 (grid/parse example-grid))
(answer-part2 (grid/parse-file "2023/day16.txt"))


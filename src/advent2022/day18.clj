(ns advent2022.day18
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [str]
  (mapv utils/parse-int (.split str ",")))

(def lines (utils/read-resource-lines "input/day18.txt"))

(defn cube-vertices [[x y z]]
  (for [x [x (inc x)]
        y [y (inc y)]
        z [z (inc z)]]
    [x y z]))

(defn compare-vertices [[x0 & xs0] [x1 & xs1]]
  (cond (= x0 x1) (compare-vertices xs0 xs1)
        :else (compare x0 x1)))

(defn cube-faces [[x y z]]
  (let [vertices (cube-vertices [x y z])]
    (->> (combo/combinations vertices 4)
         (filter
          (fn [vs]
            (some true? (map (fn [x] (apply = x))
                             (for [n (range 0 3)]
                               (map (fn [v] (nth v n)) vs))))))

         (map (partial sort compare-vertices)))))

(count (cube-faces [1 1 2]))

(defn count-faces [acc cube]
  (->>
   (cube-faces cube)
   (reduce
    (fn [acc face]
      (-> acc
          (update-in [face] (fnil inc 0))))
    acc)))

(defn surface-area [cubes]
  (->> cubes
       (reduce count-faces {})
       (vals)
       (filter #(= % 1))
       (count)))

;; answer to part 1
(surface-area (map parse-line lines))

;; for part 2, I think we will just flood fill the area.

(defn coord-bounds [cubes]
  (reduce
   (fn [[[xmin xmax] [ymin ymax] [zmin zmax]] [x y z]]
     [[(min xmin x) (max xmax x)]
      [(min ymin y) (max ymax y)]
      [(min zmin z) (max zmax z)]])
   (repeat 3 [Integer/MAX_VALUE Integer/MIN_VALUE])
   cubes))

(defn in-bounds? [bounds cube]
  (every?
   true?
   (for [n (range 0 3)]
     (let [v (nth cube n)]
       (and (>= v (first (nth bounds n)))
            (<= v (second (nth bounds n))))))))

(defn traverse
  "traverse squares from this cube"
  [[x y z]]
  (->> (for [dx [-1 1]
             dy [-1 1]
             dz [-1 1]]
         (list
          [(+ x dx) y z]
          [x (+ y dy) z]
          [x y (+ z dz)]))
       (apply concat)
       (distinct)))

(defn expand-bounds [n bounds]
  (map #(vector (- (first %) n) (+ (second %) n)) bounds))

(defn flood-fill [cubes]
  (let [cube-set (set cubes)
        bounds (expand-bounds 1 (coord-bounds cubes))]
    (loop [[queue visited num-blocked] [[(map first bounds)] #{} 0]]
      (if (empty? queue)
        num-blocked
        (recur
         (reduce
         (fn [[queue visited num-blocked] neighbor]
           (cond
             (contains? cube-set neighbor) [queue visited (inc num-blocked)]
             (contains? visited neighbor) [queue visited num-blocked]
             :else [(conj queue neighbor) (conj visited neighbor) num-blocked]))
         [(subvec queue 1) visited num-blocked]
         (filter (partial in-bounds? bounds) (traverse (nth queue 0)))))))))

(println "answer to part 2" (flood-fill (map parse-line lines)))

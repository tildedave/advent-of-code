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

(use 'clojure.tools.trace)
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

;; for part 2, we need to find every cube defined by the faces
;; and see if it was one of the original vertices

;; a vertex that has faces that are totally inside the faces, but
;; itself is NOT in the list of original vertices.

(defn enclosed-cubes [cubes]
  (let [cube-set (set cubes)
        exposed-faces (->> cubes
                           (reduce count-faces {})
                           (filter #(= (second %) 1))
                           (map first)
                           (set))
        vertices (apply concat exposed-faces)]
    (->> vertices
         (remove (partial contains? cube-set))
         (map cube-faces)
         (distinct)
         (filter (fn [face-vertices] (every? #(contains? exposed-faces %) face-vertices))))))

enclosed-cubes

(enclosed-cubes (map parse-line lines))

(defn count-faces-from-enclosed [acc cube]
  (->>
   cube
   (reduce
    (fn [acc face]
      (-> acc
          (update-in [face] (fnil inc 0))))
    acc)))

(let [foo (->> (map parse-line lines)
               (enclosed-cubes)
               (reduce count-faces-from-enclosed {})
               (filter #(= (second %) 1)))
      bar (->> (map parse-line lines)
           (reduce count-faces {})
           (filter #(= (second %) 1)))]
  (println (count foo))
  (println (count bar))
  (println "difference" (count (set/difference (set bar) (set foo)))))

(->> (map parse-line lines)
     (enclosed-cubes)
     (reduce count-faces-from-enclosed {})
     (vals)
     (filter #(= % 1))
     (count)
     (- (surface-area (map parse-line lines))))

(- 3498 174)

(reduce count-faces-from-enclosed {} (enclosed-cubes (map parse-line lines)))
       (vals)

  (count)))



(defn exposed-surface-area [cubes]
  (let [area (surface-area cubes)
        num-enclosed-cubes (enclosed-cubes cubes)]
    (- area (* num-enclosed-cubes 6))))

(println "exposed surface area" (exposed-surface-area (map parse-line lines)))

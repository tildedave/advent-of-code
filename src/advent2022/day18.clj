(ns advent2022.day18
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(defn parse-line [str]
  (mapv utils/parse-int (.split str ",")))

(def lines (utils/read-resource-lines "input/day18-example.txt"))

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

;; for part 2, I think we will just flood fill the faces to find
;; size of each component.
;; it's not clear which of these will be the outside, we might have
;; to do a little more work.

;; a face is connected to another face if they share an edge.
;; so, have face, see all the edges, see if those edges are in a face.
;; I guess we store map where edges -> face.
;; I wonder if there is some way in Clojure to de-duplicate key -> values.

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

(enclosed-cubes (map parse-line lines))

(def enclosed-example-faces
  '((([2 2 5] [2 2 6] [2 3 5] [2 3 6])
    ([2 2 5] [2 2 6] [3 2 5] [3 2 6])
    ([2 2 5] [2 3 5] [3 2 5] [3 3 5])
    ([2 2 6] [2 3 6] [3 2 6] [3 3 6])
    ([2 3 5] [2 3 6] [3 3 5] [3 3 6])
    ([3 2 5] [3 2 6] [3 3 5] [3 3 6]))))

(defn exposed-faces [cubes]
  (->> cubes
       (reduce count-faces {})
       (filter #(= (second %) 1))
       (keys)))

(exposed-faces (map parse-line lines))

(defn vertices-adjacent? [[x0 y0 z0] [x1 y1 z1]]
  (or (and (not= x0 x1) (= y0 y1) (= z0 z1))
      (and (= x0 x1) (not= y0 y1) (= z0 z1))
      (and (= x0 x1) (= y0 y1) (not= z0 z1))))

(defn face-to-edges [[v1 v2 v3 v4]]
  (filter
   #(apply vertices-adjacent? %)
   (combo/combinations [v1 v2 v3 v4] 2)))

(combo/combinations [1 2 3 4] 2)

[(face-to-edges [[3 3 2] [3 3 3] [4 3 2] [4 3 3]])
(face-to-edges [[3 3 2] [3 3 3] [3 4 2] [3 4 3]])]


(defn traversal-graph [faces]
  (reduce
   (fn [acc face]
     (reduce
      (fn [acc edge]
        (update acc edge #(conj (or % #{}) face)))
      acc
      (map #(sort compare-vertices %) (face-to-edges face))))
   {}
   faces))


;; [2 2 5] [2 2 6] [2 3 5] [2 3 6] is an interior face.
(def graph (traversal-graph (exposed-faces (map parse-line lines))))

(count (exposed-faces (map parse-line lines)))
(+ (* 9 6) 6)

(defn travel-graph [cubes]
  (let [faces (exposed-faces cubes)
        graph (traversal-graph faces)]
  ;; this is just BFS/DFS, we'll start with a face,
  ;; get its neighbors through the traversal graph, etc
  ;; end result will be sets of faces.
  ;; we'll need some test to understand which of these
  ;; faces is on the "outside" but maybe that will be very clear.
  ;; I guess the one with the most # of faces has to be it :-)
    (loop [visited #{}
           components []]
      (let [start-candidates (remove #(contains? visited %) faces)]
        (println "components" components)
        (if (empty? start-candidates)
          components
          (let [start (first start-candidates)
                component
                (loop [queue [start]
                       visited #{}]
                  (if (empty? queue)
                    visited
                    (let
                     [[face] queue
                      _ (println "visiting" face)
                      [queue visited]
            ;;   queue (subvec queue 1)]
           ;; add stuff to the queue
                      (reduce
                       (fn [[queue visited] face]
                         (if (contains? visited face)
                           [queue visited]
                           (do (println "adding" face "to next queue") [(conj queue face) (conj visited face)])))
                       [(subvec queue 1) visited]
                       (distinct (mapcat graph (face-to-edges face))))]
                      (recur queue visited))))]
            (recur
             ;; add everything in component to visited, so we don't return.
             (set/union visited component)
             (conj components component))))))))

;; so this does not give me two connected components :/
(println "travel graph" (map count (travel-graph (map parse-line lines))))

;; for part 2, we need to find every cube defined by the faces
;; and see if it was one of the original vertices

;; a vertex that has faces that are totally inside the faces, but
;; itself is NOT in the list of original vertices.

;; enclosed-cubes

;; (enclosed-cubes (map parse-line lines))

;; (defn count-faces-from-enclosed [acc cube]
;;   (->>
;;    cube
;;    (reduce
;;     (fn [acc face]
;;       (-> acc
;;           (update-in [face] (fnil inc 0))))
;;     acc)))

;; (let [foo (->> (map parse-line lines)
;;                (enclosed-cubes)
;;                (reduce count-faces-from-enclosed {})
;;                (filter #(= (second %) 1)))
;;       bar (->> (map parse-line lines)
;;            (reduce count-faces {})
;;            (filter #(= (second %) 1)))]
;;   (println (count foo))
;;   (println (count bar))
;;   (println "difference" (count (set/difference (set bar) (set foo)))))

;; (->> (map parse-line lines)
;;      (enclosed-cubes)
;;      (reduce count-faces-from-enclosed {})
;;      (vals)
;;      (filter #(= % 1))
;;      (count)
;;      (- (surface-area (map parse-line lines))))

;; (- 3498 174)

;; (reduce count-faces-from-enclosed {} (enclosed-cubes (map parse-line lines)))
;;        (vals)

;;   (count)))



;; (defn exposed-surface-area [cubes]
;;   (let [area (surface-area cubes)
;;         num-enclosed-cubes (enclosed-cubes cubes)]
;;     (- area (* num-enclosed-cubes 6))))

;; (println "exposed surface area" (exposed-surface-area (map parse-line lines)))

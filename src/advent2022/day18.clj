(ns advent2022.day18
  (:require [advent2022.utils :as utils]
            [clojure.math.combinatorics :as combo]))

(defn cube-vertices [[x y z]]
  (for [x [x (inc x)]
          y [y (inc y)]
          z [z (inc z)]]
      [x y z]))

(defn vertices-adjacent? [[x0 y0 z0] [x1 y1 z1]]
  (or (and (not= x0 x1) (= y0 y1) (= z0 z1))
      (and (= x0 x1) (not= y0 y1) (= z0 z1))
      (and (= x0 x1) (= y0 y1) (not= z0 z1))))

(defn cube-edges [cube]
  ;; a cube comes with 8 vertices.
  ;; these become the unique edges.
  ;; the edges being deduplicated requires a canonical order between them.
  ;; let's do the codez and see what pops out.
  ;; we need to take a stand on dec.  I guess no dec.
  ;; coords are kind of fuzzy but this will get part 1 at least.
  (filter
   #(apply vertices-adjacent? %)
   (combo/combinations (cube-vertices cube) 2)))

;; normalize our edges so we don't have to double-count
(defn compare-vertices [[x0 & xs0] [x1 & xs1]]
  (cond (= x0 x1) (compare-vertices xs0 xs1)
        :else (compare x0 x1)))


;; this is wrong but it can be made right.
;; what we need is to keep track of existing edges and merge them into the cube.
;; if we were looking to add an edge, and the edge is already there, delete it.
;; the current logic will remove edges that are actually exposed.
;; we want to be face-oriented instead.
;; then don't need to do this v - e + f = 2 dance.
;; we will normalize the faces so each face can be uniquely detected.

(defn count-edges [acc cube]
  (reduce
   (fn [acc edge]
     (-> acc
      (update-in [edge] (fn [old-val] (if (nil? old-val) 1 nil)))))
   acc
   (map (partial sort compare-vertices) (cube-edges cube))))

(reduce count-edges {} [[1 1 1] [2 1 1]])

;; this double-counts edges.
;; OK, so we get # edges.
;; from that we get # exposed vertices.
;; result of v - e + f = 2 -> solving for f.

(defn surface-area [cubes]
  ;; this double-counts edges
  (let [exposed-edges (filter #(not= (second %) nil) (reduce count-edges {} cubes))
        exposed-vertices (reduce (fn [acc [[e1 e2]]]
                                   (-> acc (conj e1) (conj e2)))
                                 (set (list))
                                 exposed-edges)]
    [(count exposed-vertices) (count exposed-edges)]))

(let [[v e] (surface-area [[1 1 1] [2 1 1]])]
  [v e])

(count (filter #(= (second %) 1) (count-edges (count-edges {} [1 1 1]) [2 1 1])))


(ns advent2022.day17
  (:require [advent2022.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as string]))

;; y = 0 is the bottom
;; shapes appear 3 y units above highest (so keep track of the highest)
;; question: difference between the at-rest rocks and the shape.
;; I guess we can just do the basic stuff and see how it goes.
;; keep track of the rows of the active shape.

(def shape-order (list :horizontal :plus :bend :vertical :square))

(def rock-cavern [])

(defn new-row [] (vec (repeat 7 \.)))
(new-row)

(defn add-shape [shape rock-cavern]
  (let [start-y (+ (count rock-cavern) 3)
        expanded-cavern (conj rock-cavern (new-row) (new-row) (new-row) (new-row))]
    (reduce
     (fn [m r] (assoc-in m r \@))
     expanded-cavern
     (case shape
       :horizontal
       [[start-y 2] [start-y 3] [start-y 4] [start-y 5]]
       :plus
       [[start-y 3] [(- start-y 1) 2] [(- start-y 1) 3] [(- start-y 1) 4] [(- start-y 2) 3]]
       :bend
       [[start-y 4] [(- start-y 1) 4] [(- start-y 2) 4] [(- start-y 2) 3] [(- start-y 2) 2]]
       :vertical
       [[start-y 2] [(- start-y 1) 2] [(- start-y 2) 2] [(- start-y 3) 2]]
       :square
       [[start-y 2] [start-y 3] [(- start-y 1) 2] [(- start-y 1) 3]]))))

(defn visualize-cavern [rock-cavern]
  (string/join "\n" (map string/join (reverse rock-cavern))))

(defn every-index-of [v item]
  ;; bad perf but failed at trying to fight lazy-seq and the indices we're dealing with are small
  (->> v
       (map-indexed vector)
       (filter #(= (second %) item))
       (map first)))

(every-index-of ((add-shape :square rock-cavern) 3) \@)

(range 0 5)
(defn every-index-in-range [rock-cavern ystart yend ch]
  (mapcat
   #(map (fn [i] (vector i %)) (every-index-of (rock-cavern %) ch))
   (range ystart (inc yend))))

(every-index-of ((add-shape :vertical rock-cavern) 3) \@)
(every-index-in-range (add-shape :vertical rock-cavern) 0 3 \@)

(defn update-locations [rock-cavern coord-list ch]
  (reduce
   (fn [m r] (assoc-in m (reverse r) ch))
   rock-cavern
   coord-list))

(set (list 4 5 6))
;; (defn fall [rock-cavern ystart yend]
;;   ;; ystart/yend are where we look for any @s.
;;   ;; there are going to be a lot of rows and we only want to look at a few.
;;   (let [shape-locations (every-index-in-range rock-cavern ystart yend \@)
;;         rock-locations (every-index-in-range rock-cavern ystart yend \#)
;;         fall-locations (map (fn [[x y]] [x (dec y)]) shape-locations)
;;         fall-blocked (or (some #(= (second %) 0) fall-locations)
;;                          (not-empty (set/intersection (set rock-locations) (set fall-locations))))]
;;     [fall-blocked
;;      (if fall-blocked
;;        (update-locations rock-cavern shape-locations \#)
;;        (-> rock-cavern
;;            (update-locations shape-locations \.)
;;            (update-locations fall-locations \@)))]))

(defn out-of-bounds? [[x y]]
  (or (= y 0)
      (< x 0)
      (> x 6)))

(defn move-shape [rock-cavern ystart yend direction rock?]
  (let [shape-locations (every-index-in-range rock-cavern ystart yend \@)
        rock-locations (every-index-in-range rock-cavern ystart yend \#)
        next-locations (map
                        (fn [[x y]]
                          [(case direction :right (inc x) :left (dec x) x)
                           (case direction :down (dec y) y)])
                        shape-locations)
        next-blocked (or (some out-of-bounds? next-locations)
                         (not-empty (set/intersection (set rock-locations) (set next-locations))))]
    [next-blocked
     (if next-blocked
      (if rock? (update-locations rock-cavern shape-locations \#) rock-cavern)
      (-> rock-cavern
             (update-locations shape-locations \.)
             (update-locations next-locations \@)))]))

(defn fall [rock-cavern ystart yend] (move-shape rock-cavern ystart yend :down true))
(defn gust [rock-cavern ystart yend direction] (move-shape rock-cavern ystart yend direction false))

;;   (reduce
;;    (fn [[can-fall acc] y]
;;      (let [idxs (every-index-of (acc y) \@)
;;            can-fall (and can-fall (not= y 0) (every? #(= ((acc (dec y)) %) \.) idxs))]
;;        [can-fall
;;         (if can-fall
;;           (-> acc
;;               (assoc y
;;                      (reduce
;;                       (fn [row idx] (assoc row idx \.))
;;                       (acc y)
;;                       idxs))
;;                ;; actually we must test to see if we CAN fall which means all the indices we find are clear.
;;                ;; in the event that we can't fall, we want to turn into #.
;;               (assoc (dec y)
;;                      (reduce
;;                       (fn [row idx] (assoc row idx \@))
;;                       (acc (dec y))
;;                       idxs)))
;;           (-> acc
;;               (assoc y
;;                      (reduce
;;                       (fn [row idx] (assoc row idx \#))
;;                       (acc y)
;;                       idxs))))]))
;;        [true rock-cavern]
;;        (range ystart (inc yend))))

;; oh yeah, we also have to do jets pushing the shape
;; this is comparably easy in the grand scheme of things.

(println (visualize-cavern (second (fall (add-shape :vertical rock-cavern) 0 3))))

(println (visualize-cavern (second (fall (add-shape :horizontal rock-cavern) 0 3))))

      (println (visualize-cavern (add-shape :vertical rock-cavern)))

(ns grid
  (:require [utils :as utils]))

(defn parse
  ([lines] (parse lines identity))
  ([lines sq-f]
   (->> lines
        (map seq)
        (map #(mapv sq-f %))
        (vec))))

(defn parse-file
  ([file] (parse-file file identity))
  ([file sq-f] (-> file
                   (utils/read-input)
                   (parse sq-f))))

(defn bounds [grid]
  [(count (first grid))
   (count grid)])

(defn at [grid [x y]]
  (get-in grid [y x]))

(defn assoc [grid [x y] k]
  (assoc-in grid [y x] k))

(defn make [xmax ymax empty-sq]
  (vec (for [y (range 0 ymax)]
    (vec
     (for  [x (range 0 xmax)]
      empty-sq
      )))))

(defn out-of-bounds? [grid [x y]]
  (nil? (at grid [x y])))
  ;; (or (< y 0)
  ;;     (>= y (count grid))
  ;;     (< x 0) (>= x (count (first grid)))))

(defn in-bounds? [grid [x y]]
  (not (out-of-bounds? grid [x y])))

(def cardinal-directions [[-1 0] [1 0] [0 -1] [0 1]])
(def ordinal-directions [[-1 -1] [-1 1] [1 -1] [1 1]])
(def all-directions (concat cardinal-directions ordinal-directions))

(defn add [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn neighbors
  ([grid [x y]] (neighbors grid [x y] cardinal-directions))
  ([grid [x y] directions] (neighbors grid [x y] directions (fn [_] false)))
  ([grid [x y] directions is-wall?]
   (remove
    nil?
    (for [[dx dy] directions]
      (let [[nx ny] [(+ x dx) (+ y dy)]]
        (cond
          (out-of-bounds? grid [nx ny]) nil
          (is-wall? (at grid [nx ny])) nil
          :else [nx ny]))))))

(defn coords [grid]
  (let [xmax (reduce max (map count grid))
        ymax (count grid)]
    (for [x (range 0 xmax)
          y (range 0 ymax)]
      [x y])))

(defn is-adjacent?
  ([[x1 y1] [x2 y2]] (is-adjacent? [x1 y1] [x2 y2] cardinal-directions))
  ([[x1 y1] [x2 y2] directions]
   (->>
    (for [[dx dy] directions]
      (if (= [(+ x1 dx) (+ y1 dy)] [x2 y2])
        true
        false))
    (filter true?)
    (empty?)
    (not))))

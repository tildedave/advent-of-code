(ns advent2022.day24
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]))

(def example-lines (utils/read-resource-lines "input/day24-example.txt"))
(def example2-lines (utils/read-resource-lines "input/day24-example2.txt"))

(defn parse-grid [lines]
  (->> lines
       (mapv vec)))

(defn string-grid [grid]
  (string/join "\n" (map string/join grid)))

(defn is-blizzard? [ch]
  (case ch
    \> true
    \< true
    \v true
    \^ true
    false))

(defn blizzard-positions [grid]
  (->> grid
       (map-indexed vector)
       (map (fn [[y x-list]] [y (->> x-list
                                     (map-indexed vector)
                                     (filter #(is-blizzard? (second %))))]))
       ;; must handle multiple rows.
       (remove #(empty? (second %)))
       (mapcat (fn [[y x-list]]
                 (map (fn [[x ch]] [[x y] #{ch}]) x-list)))
       (into {})))

(blizzard-positions (parse-grid example-lines))
    ;;    (map-indexed vector)))

    ;;    (filter #(is-blizzard? (second %)))))

(defn reverse-blizzard [ch]
  (case ch
    \^ \v
    \v \^
    \< \>
    \> \<))

#{\^}

(defn tick [grid positions]
  (reduce
   (fn [positions [[x y] ch]]
     (let [[dx dy] (case ch
                     \^ [0 -1]
                     \> [1 0]
                     \< [-1 0]
                     \v [0 1])
           [nx ny] [(+ x dx) (+ y dy)]
           [nx ny] (if (= (get-in grid [ny nx]) \#)
                     [(mod (+ x (* 3 dx)) (count (first grid)))
                      (mod (+ y (* 3 dy)) (count grid))]
                     [nx ny])]
       (-> positions
           (update [x y] (fn [s] (disj s ch)))
           (update [nx ny] (fn [s] (conj (if (nil? s) #{} s) ch))))))
   positions
   (apply concat
          (for [[x y] (keys positions)]
            (for [ch (positions [x y])]
              [[x y] ch])))))

(defn print-grid [grid positions]
  (->> (for [y (range 0 (count grid))]
         (for [x (range 0 (count (first grid)))]
           (if
            (= (get-in grid [y x]) \#) \#
            (let [pos-set (get positions [x y] #{})]
              (condp = (count pos-set)
                0 \.
                1 (first pos-set)
                (count pos-set))))))
       (map string/join)
       (string/join "\n")
       (println)))

(defn blizzard-seq [lines]
  (let [g (parse-grid lines)]
    (iterate #(vector g (apply tick %)) [g (blizzard-positions g)])))

(apply print-grid
       (-> (blizzard-seq example2-lines)
           (nth 2)))

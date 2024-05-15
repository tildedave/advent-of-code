(ns advent2017.day19
  (:require [grid :as grid]
            [utils :as utils]
            [clojure.string :as string]))

(defn parse-grid [filename]
  (grid/parse (utils/read-input filename)))

(parse-grid "2017/day19-example.txt")

(defn starting-state [grid]
  {:coords
   (->> (grid/coords grid)
        (filter (fn [[x y]] (and (= y 0) (= (grid/at grid [x y]) \|))))
        (first))
   :direction :down
   :seen []
   :steps 0})

(starting-state (parse-grid "2017/day19-example.txt"))

(defn walk-direction [grid]
  (fn [[x y] direction]
    (let [[dx dy] (case direction
                    :up [0 -1]
                    :down [0 1]
                    :left [-1 0]
                    :right [1 0])]
      [(+ x dx) (+ y dy)])))

(defn reverse-direction [direction]
  (case direction
    :up :down
    :down :up
    :left :right
    :right :left))

(defn walk-grid [grid]
  (let [walk-direction (walk-direction grid)]
    (fn [state]
      (let [{:keys [coords direction]} state
            ch (grid/at grid coords)
            direction (if (= ch \+)
                        (->> (list :up :down :left :right)
                             (remove #(= % (reverse-direction direction)))
                             (remove #(let [c (walk-direction coords %)]
                                        (or (nil? (grid/at grid c))
                                            (= \space (grid/at grid c))
                                            )))
                             (first))
                        direction)
            coords (walk-direction coords direction)
            done? (or (nil? ch) (= (grid/at grid coords) \space))]
        (-> state
            (update :steps inc)
            (update :seen #(cond (nil? ch) %
                                 (Character/isAlphabetic (int ch)) (conj % ch)
                                 :else %))
            (assoc :direction direction)
            (assoc :done? done?)
            (update :coords #(if done? % coords)))))))

(defn answer [filename]
  (let [grid (parse-grid filename)]
    (reduce
     (fn [acc {:keys [done? seen]}]
       (if done?
         (reduced (string/join seen))
         acc))
     nil
     (iterate (walk-grid grid) (starting-state grid)))))

(println (answer "2017/day19.txt"))

(defn answer-part2 [filename]
  (let [grid (parse-grid filename)]
    (reduce
     (fn [acc {:keys [done? steps]}]
       (if done?
         (reduced steps)
         acc))
     nil
     (iterate (walk-grid grid) (starting-state grid)))))

(println (answer-part2 "2017/day19.txt"))

;; (println (let [grid (parse-grid "2017/day19.txt")]
;;   (iterate (walk-grid grid) (starting-state grid))))

(Character/isAlphabetic (int \space))

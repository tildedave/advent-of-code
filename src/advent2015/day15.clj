(ns advent2015.day15
  (:require [utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

;; (def ingredient-re #"^(\w+): (((?:, )?(\w+) (-?\d+))*)$")

;; (re-matches ingredient-re "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")

;; , durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")

(defn parse-ingredient [^String line]
  (let [[ingredient stats] (.split line ": ")]
    {ingredient (->> (.split stats ", ")
                     (map #(.split % " "))
                     (map #(mapv utils/try-parse-int %))
                     (into {}))}))

(parse-ingredient "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")

(->> (utils/read-input "2015/day15-example.txt")
     (map parse-ingredient)
     (reduce merge))

(defn breakdowns [max num-ingredients]
  (if (= num-ingredients 1) (list (list max))
      (apply concat
             (for [n (range max)]
        (map
         #(cons n %)
         (breakdowns (- max n) (dec num-ingredients)))))))

(defn score [ingredient-props ingredient-quantity-map]
  (let [vs (->>
           (for [[ingredient quantity] ingredient-quantity-map]
             (->>
              (for [[k c] (ingredient-props ingredient)]
                {k (* c quantity)})
              (apply merge)))
           (apply merge-with +)
           (#(dissoc % "calories"))
           (vals))]
   (if (empty? (filter #(< % 0) vs))
     (reduce * vs)
     0)))

;;    (vals)
;;    (reduce *)))

 (let [ingredient-props (->> "2015/day15-example.txt"
                            (utils/read-input)
                            (map parse-ingredient)
                            (reduce merge))]
   (score ingredient-props {"Butterscotch" 44 "Cinnamon" 56}))


(defn answer-part1 [filename]
  (let [ingredient-props (->> (format "2015/%s" filename)
                              (utils/read-input)
                              (map parse-ingredient)
                              (reduce merge))
        ingredients (vec (keys ingredient-props))
        ingredient-breakdowns (->> (breakdowns 100 (count ingredients))
                                   (map #(map-indexed (fn [n quantity] {(get ingredients n 0) quantity}) %))
                                   (map #(apply merge %))
                                   )]
    (->> ingredient-breakdowns
         (map (partial score ingredient-props))
         (sort >)
         (first)
         )))

(answer-part1 "day15-example.txt")
(answer-part1 "day15.txt")


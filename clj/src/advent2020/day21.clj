(ns advent2020.day21
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.core.logic :refer [run* conde == !=]]))

('clojure.core.logic/lvar "mxmxvkd")

(def allergen-re #"^((\s?\w+)+) \(contains ([^\)]+)\)$")

(re-matches allergen-re "mxmxvkd kfcds sqjhc nhms (contains soy, fish)")

(defn parse-allergen-line [line]
  (let [[_ ingredients _ allergens] (re-matches allergen-re line)]
    {:ingredients (set (^String .split ingredients " "))
     :allergens (set (^String .split allergens ", "))}))

(parse-allergen-line "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)")

(->> (utils/read-input "2020/day21-example.txt")
     (map parse-allergen-line))

(defn all-items [kw parsed-lines]
  (reduce
   (fn [acc l] (reduce conj acc (kw l)))
   #{}
   parsed-lines))

(->> (utils/read-input "2020/day21-example.txt")
     (map parse-allergen-line)
     (all-items :ingredients))

;; part 1:
;; for each allergen, take intersection of all ingredients.
;; any ingredient that is no intersection is the one we care about.

(defn ingredient-intersection-set [parsed-lines]
  (update-vals
   (reduce
    (fn [acc parsed-line]
      (reduce
       (fn [acc allergen]
         (update acc allergen (fnil conj []) (parsed-line :ingredients)))
       acc
       (:allergens parsed-line)))
    {}
    parsed-lines)
   #(apply set/intersection %)))

(defn answer-part1 [filename]
  (let [parsed-lines (->> filename
                          (format "2020/%s")
                          (utils/read-input)
                          (map parse-allergen-line))
        non-allergens (->> parsed-lines
                           (ingredient-intersection-set)
                           (vals)
                           (apply set/union)
                           (set/difference (all-items :ingredients parsed-lines)))]
    ;; number of times the non-allergens appear.
    (reduce
     (fn [acc parsed-line]
       (+ acc (count (set/intersection non-allergens (parsed-line :ingredients))))
       )
     0
     parsed-lines)))

(answer-part1 "day21-example.txt")
(answer-part1 "day21.txt")

(defn allergen-search [parsed-lines]
  (loop
   [intersections (ingredient-intersection-set parsed-lines)
    allergen-mapping {}]
    (if (empty? intersections)
      allergen-mapping
      (let [[allergen vs] (->> intersections
                               (filter (fn [[x s]] (= (count s) 1)))
                               (first))
            ingredient (first vs)]
        (recur
         (-> intersections
             (update-vals #(disj % ingredient))
             (dissoc allergen))
         (merge allergen-mapping {allergen ingredient})))))
  )

(defn answer-part2 [filename]
  (->> filename
       (format "2020/%s")
       (utils/read-input)
       (map parse-allergen-line)
       (allergen-search)
       (sort-by first)
       (map second)
       (string/join ",")))

(answer-part2 "day21-example.txt")
(answer-part2 "day21.txt")
;; this is basically trivial .... I can almost do it by hand.

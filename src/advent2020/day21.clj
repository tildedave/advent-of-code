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

(def ingredient-intersection-set [parsed-lines]
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
                          (map parse-allergen-line))]
    ))

(answer-part1 "day21-example.txt")

allergens (all-items :allergens parsed-lines)]
    (group-by )
    parsed-lines))


;; OK, we could potentially go back to core.logic after this initial pruning.
;; let's keep the code for it.

(defn every-allergen-has-a-single-ingredient [allergen ingredient-set]
      ;; this returns a single constraint.
  (str
   "(conde"
   "\n\t"
   (str
    (string/join
     "\n\t"
     (for [ingredient ingredient-set]
       (str "["
            (let [other-ingredients (disj ingredient-set ingredient)]
              (string/join " "
                           (conj
                            (for [other-ingredient other-ingredients]
                              (format "(!= %s \"%s\")" other-ingredient allergen))
                            (format "(== %s \"%s\")" ingredient allergen))))
            "]"))))
   ")"))

(defn individual-allergen-ingredients-constraints [parsed-line]
      ;; for each allergen, it must be one of the ingredients.
      ;; so this returns a list of constraints.
  (for [allergen (:allergens parsed-line)]
    (str
     "(conde"
     (string/join
      " "
      (for [ingredient (:ingredients parsed-line)]
        (format "[(== %s \"%s\")]" ingredient allergen)))
     ")")))


(defn create-logic-statement [filename]
      ;; we'll just create the core.logic statement and call eval() on it
      ;; there's probably a cooler way.
  (let [parsed-lines (->> (utils/read-input (format "2020/%s" filename))
                          (map parse-allergen-line))
        ingredients (all-items :ingredients parsed-lines)
        allergens (all-items :allergens parsed-lines)]
             ;; each allergen has one and only one ingredient for it.
             ;; then, for each line, a cond statement for it.
    (str (format "(run* [%s]\n" (string/join " " (seq ingredients)))
         (string/join
          "\n"
          (for [allergen allergens]
            (every-allergen-has-a-single-ingredient allergen ingredients)))
         "\n"
         (string/join
          "\n"
          (apply concat
                 (for [parsed-line parsed-lines]
                   (individual-allergen-ingredients-constraints parsed-line))))
         ")")))

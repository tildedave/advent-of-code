(ns advent2020.day21
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.core.logic :refer [run* conde == !=]]))

('clojure.core.logic/lvar "mxmxvkd")

(def allergen-re #"^((\s?\w+)+) \(contains ([^\)]+)\)$")

(re-matches allergen-re "mxmxvkd kfcds sqjhc nhms (contains soy, fish)")

(defn parse-allergen-line [line]
  (let [[_ ingredients _ allergens] (re-matches allergen-re line)]
    {:ingredients (^String .split ingredients " ")
     :allergens (^String .split allergens ", ")}))

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

(for [a #{1 2 3}] a)

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
            "]")
            )))
   ")"
   ))
(println (every-allergen-has-a-single-ingredient "dairy" #{1 2 3}))

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

(individual-allergen-ingredients-constraints
 {:ingredients ["sqjhc", "fvjkl"], :allergens ["soy" "dairy"]}
 )

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
            (individual-allergen-ingredients-constraints parsed-line)
            )))
         ")"
         )
  ))

(println (create-logic-statement "day21.txt"))

;; OK so this works.  cool.
;; (run* [sqjhc fvjkl nhms trh kfcds sbzzf mxmxvkd]
;;  (conde
;;  	[(== sqjhc "dairy") (!= fvjkl "dairy") (!= nhms "dairy") (!= trh "dairy") (!= kfcds "dairy") (!= sbzzf "dairy") (!= mxmxvkd "dairy")]
;;  	[(== fvjkl "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= kfcds "dairy") (!= sbzzf "dairy") (!= mxmxvkd "dairy")]
;;  	[(== nhms "dairy") (!= sqjhc "dairy") (!= fvjkl "dairy") (!= trh "dairy") (!= kfcds "dairy") (!= sbzzf "dairy") (!= mxmxvkd "dairy")]
;;  	[(== trh "dairy") (!= sqjhc "dairy") (!= fvjkl "dairy") (!= nhms "dairy") (!= kfcds "dairy") (!= sbzzf "dairy") (!= mxmxvkd "dairy")]
;;  	[(== kfcds "dairy") (!= sqjhc "dairy") (!= fvjkl "dairy") (!= nhms "dairy") (!= trh "dairy") (!= sbzzf "dairy") (!= mxmxvkd "dairy")]
;;  	[(== sbzzf "dairy") (!= sqjhc "dairy") (!= fvjkl "dairy") (!= nhms "dairy") (!= trh "dairy") (!= kfcds "dairy") (!= mxmxvkd "dairy")]
;;  	[(== mxmxvkd "dairy") (!= sqjhc "dairy") (!= fvjkl "dairy") (!= nhms "dairy") (!= trh "dairy") (!= kfcds "dairy") (!= sbzzf "dairy")])
;;  (conde
;;  	[(== sqjhc "soy") (!= fvjkl "soy") (!= nhms "soy") (!= trh "soy") (!= kfcds "soy") (!= sbzzf "soy") (!= mxmxvkd "soy")]
;;  	[(== fvjkl "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= kfcds "soy") (!= sbzzf "soy") (!= mxmxvkd "soy")]
;;  	[(== nhms "soy") (!= sqjhc "soy") (!= fvjkl "soy") (!= trh "soy") (!= kfcds "soy") (!= sbzzf "soy") (!= mxmxvkd "soy")]
;;  	[(== trh "soy") (!= sqjhc "soy") (!= fvjkl "soy") (!= nhms "soy") (!= kfcds "soy") (!= sbzzf "soy") (!= mxmxvkd "soy")]
;;  	[(== kfcds "soy") (!= sqjhc "soy") (!= fvjkl "soy") (!= nhms "soy") (!= trh "soy") (!= sbzzf "soy") (!= mxmxvkd "soy")]
;;  	[(== sbzzf "soy") (!= sqjhc "soy") (!= fvjkl "soy") (!= nhms "soy") (!= trh "soy") (!= kfcds "soy") (!= mxmxvkd "soy")]
;;  	[(== mxmxvkd "soy") (!= sqjhc "soy") (!= fvjkl "soy") (!= nhms "soy") (!= trh "soy") (!= kfcds "soy") (!= sbzzf "soy")])
;;  (conde
;;  	[(== sqjhc "fish") (!= fvjkl "fish") (!= nhms "fish") (!= trh "fish") (!= kfcds "fish") (!= sbzzf "fish") (!= mxmxvkd "fish")]
;;  	[(== fvjkl "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= kfcds "fish") (!= sbzzf "fish") (!= mxmxvkd "fish")]
;;  	[(== nhms "fish") (!= sqjhc "fish") (!= fvjkl "fish") (!= trh "fish") (!= kfcds "fish") (!= sbzzf "fish") (!= mxmxvkd "fish")]
;;  	[(== trh "fish") (!= sqjhc "fish") (!= fvjkl "fish") (!= nhms "fish") (!= kfcds "fish") (!= sbzzf "fish") (!= mxmxvkd "fish")]
;;  	[(== kfcds "fish") (!= sqjhc "fish") (!= fvjkl "fish") (!= nhms "fish") (!= trh "fish") (!= sbzzf "fish") (!= mxmxvkd "fish")]
;;  	[(== sbzzf "fish") (!= sqjhc "fish") (!= fvjkl "fish") (!= nhms "fish") (!= trh "fish") (!= kfcds "fish") (!= mxmxvkd "fish")]
;;  	[(== mxmxvkd "fish") (!= sqjhc "fish") (!= fvjkl "fish") (!= nhms "fish") (!= trh "fish") (!= kfcds "fish") (!= sbzzf "fish")])
;;  (conde[(== mxmxvkd "dairy")] [(== kfcds "dairy")] [(== sqjhc "dairy")] [(== nhms "dairy")])
;;  (conde[(== mxmxvkd "fish")] [(== kfcds "fish")] [(== sqjhc "fish")] [(== nhms "fish")])
;;  (conde[(== trh "dairy")] [(== fvjkl "dairy")] [(== sbzzf "dairy")] [(== mxmxvkd "dairy")])
;;  (conde[(== sqjhc "soy")] [(== fvjkl "soy")])
;;  (conde[(== sqjhc "fish")] [(== mxmxvkd "fish")] [(== sbzzf "fish")]))


;; ;; one day I'll learn core.logic
;; ;; just an example, I'm not sure I want to do this
;; (run*
;;  [mxmxvkd kfcds sqjhc nhms trh fvjkl sbzzf]
;;  (conde
;;   [(== mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (== kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (== sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (== nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (== trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (== fvjkl "dairy") (!= sbzzf "dairy")]
;;   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (== sbzzf "dairy")])
;;  (conde
;;   [(== mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (== kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (!= kfcds "fish") (== sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (== nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (== trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (== fvjkl "fish") (!= sbzzf "fish")]
;;   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (== sbzzf "fish")])
;;  (conde
;;   [(== mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (== kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (!= kfcds "soy") (== sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (== nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (== trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (== fvjkl "soy") (!= sbzzf "soy")]
;;   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (== sbzzf "soy")])
;;  (conde [(== mxmxvkd "dairy")] [(== kfcds "dairy")] [(== sqjhc "dairy")] [(== nhms "dairy")])
;;  (conde [(== mxmxvkd "fish")] [(== kfcds "fish")] [(== sqjhc "fish")] [(== nhms "fish")])
;;  (conde [(== trh "dairy")] [(== fvjkl "dairy")] [(== sbzzf "dairy")] [(== mxmxvkd "dairy")])
;;  (conde [(== sqjhc "soy")] [(== fvjkl "soy")])
;;  (conde [(== sqjhc "fish")] [(== mxmxvkd "fish")] [(== sbzzf "fish")]))

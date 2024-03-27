(ns advent2020.day21
  (:require [utils :as utils]))

;;  [clojure.core.logic :refer [run* conde == !=]]))

(def allergen-re #"^((\s?\w+)+) \(contains ([^\)]+)\)$")

(re-matches allergen-re "mxmxvkd kfcds sqjhc nhms (contains soy, fish)")

(defn parse-allergen-line [line]
  (let [[_ ingredients _ allergens] (re-matches allergen-re line)]
    {:ingredients (^String .split ingredients " ")
     :allergens (^String .split allergens ",")}))

(parse-allergen-line "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)")

(->> (utils/read-input "2020/day21-example.txt")
     (map parse-allergen-line))


;; one day I'll learn core.logic
;; just an example, I'm not sure I want to do this
;; these constraints aren't right.
 (run*
  [mxmxvkd kfcds sqjhc nhms trh fvjkl sbzzf]
  (conde
   [(== mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (== kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (== sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (== nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (== trh "dairy") (!= fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (== fvjkl "dairy") (!= sbzzf "dairy")]
   [(!= mxmxvkd "dairy") (!= kfcds "dairy") (!= sqjhc "dairy") (!= nhms "dairy") (!= trh "dairy") (!= fvjkl "dairy") (== sbzzf "dairy")])
  (conde
   [(== mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (== kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (!= kfcds "fish") (== sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (== nhms "fish") (!= trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (== trh "fish") (!= fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (== fvjkl "fish") (!= sbzzf "fish")]
   [(!= mxmxvkd "fish") (!= kfcds "fish") (!= sqjhc "fish") (!= nhms "fish") (!= trh "fish") (!= fvjkl "fish") (== sbzzf "fish")])
  (conde
   [(== mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (== kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (!= kfcds "soy") (== sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (== nhms "soy") (!= trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (== trh "soy") (!= fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (== fvjkl "soy") (!= sbzzf "soy")]
   [(!= mxmxvkd "soy") (!= kfcds "soy") (!= sqjhc "soy") (!= nhms "soy") (!= trh "soy") (!= fvjkl "soy") (== sbzzf "soy")])
  (conde [(== mxmxvkd "dairy")] [(== kfcds "dairy")] [(== sqjhc "dairy")] [(== nhms "dairy")])
  (conde [(== mxmxvkd "fish")] [(== kfcds "fish")] [(== sqjhc "fish")] [(== nhms "fish")])
  (conde [(== trh "dairy")] [(== fvjkl "dairy")] [(== sbzzf "dairy")] [(== mxmxvkd "dairy")])
  (conde [(== sqjhc "soy")] [(== fvjkl "soy")])
  (conde [(== sqjhc "fish")] [(== mxmxvkd "fish")] [(== sbzzf "fish")]))

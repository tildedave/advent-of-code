(ns advent2018.day5
  (:require [utils :as utils]))

(set! *warn-on-reflection* true)

(def reaction-re "(aA|bB|cC|dD|eE|fF|gG|hH|iI|jJ|kK|lL|mM|nN|oO|pP|qQ|rR|sS|tT|uU|vV|wW|xX|yY|zZ|Aa|Bb|Cc|Dd|Ee|Ff|Gg|Hh|Ii|Jj|Kk|Ll|Mm|Nn|Oo|Pp|Qq|Rr|Ss|Tt|Uu|Vv|Ww|Xx|Yy|Zz)")

(defn fully-react [^String s]
  (let [next-s (.replaceAll s reaction-re "")]
    (if (= next-s s) s
        (recur next-s))))

(count (fully-react (first (utils/read-input "2018/day5.txt"))))

(defn improve-polymer [^String s]
  (->>
   (for [n (range 26)]
    (let [ch (char (+ (int \a) n))
          upper-ch (Character/toUpperCase ch)]
      (-> s
          (.replaceAll (str ch) "")
          (.replaceAll (str upper-ch) "")
          (fully-react)
          (count))))
   (sort)
   (first)))

(improve-polymer "dabAcCaCBAcCcaDA")
;; took a while but whatever.
(improve-polymer (first (utils/read-input "2018/day5.txt")))

(ns advent2024.day11
  (:require
    [clojure.math :as math]
    [utils :as utils]))

(defn has-even-digits? [num]
  (zero? (mod (count (str num)) 2)))

(defn split-digits [num]
  (let [s (str num)
        x (/ (count s) 2)]
    [(parse-long (.substring s 0 x))
    (parse-long (.substring s x))]))

(split-digits 1233)

(defn blink [stone-list]
  (reduce
   (fn [acc num]
     (cond
       (zero? num) (conj acc 1)
       (has-even-digits? num) (let [[lower upper] (split-digits num)]
                                (-> acc
                                    (conj lower)
                                    (conj upper)))
       :else (conj acc (* num 2024))))
   []
   stone-list))

(count (first (drop 25 (iterate blink [125 17]))))

(blink [0 1 10 99 999])
(->>  [0 1 10 99 999]
;;  (
    ;;   utils/read-input "2024/day11.txt")
    ;;  (first)
    ;;  (utils/parse-number-list)
    ;;  (vec)
     (iterate blink)
     (drop 25)
     (first))

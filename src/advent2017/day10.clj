(ns advent2017.day10
  (:require [utils :as utils]))

(set! *warn-on-reflection* true)

;; it seems easiest to just always assume current-position is 0
(defn twist [{:keys [v skip-size pos]} length]
  (let [idxs (map #(mod % (count v)) (range pos (+ pos length)))
        rev-idxs (reverse idxs)]
    {:v
     (reduce
      (fn [m [orig new]]
        (assoc m new (get v orig)))
      v
      (map vector idxs rev-idxs))
     :skip-size (inc skip-size)
     :pos (mod (+ pos length skip-size) (count v))}))

(twist {:v [0 1 2 3 4] :skip-size 0 :pos 0} 3)

(defn initial-state [n]
  {:v (vec (range 0 n)) :skip-size 0 :pos 0})

(initial-state 5)

(reduce
 twist
 (initial-state 5)
 [3 4 1 5])

(defn answer [n lengths]
  (->> (reduce twist (initial-state n) lengths)
       :v
       (take 2)
       (reduce *)))

(defn read-input []
  (->> (utils/read-input "2017/day10.txt")
       (first)
       (.split #",")
       (map utils/parse-int)))

(answer 5 [3 4 1 5])
(answer 256 (read-input))

(defn read-input-p2 []
  (->> (utils/read-input "2017/day10.txt")
       (first)
       (seq)
       (map int)))

(concat
 (->> "1,2,3"
      (seq)
      (map int))
 )

(reduce bit-xor '(65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22))

(defn num-to-hex [n]
  (let [s (format "%x" n)]
    (if (= (count s) 1)
      (str "0" s)
      s)))

(defn to-dense-hash [nums]
  (->> (partition 16 nums)
       (map (partial reduce bit-xor))
       (map num-to-hex)
       (apply str)))

(defn answer-part2 [s n]
  (let [lengths (concat (->> s
                             (seq)
                             (map int))
                        '(17 31 73 47 23))
        state (initial-state n)]
    (to-dense-hash (:v (nth
     (iterate #(reduce twist % lengths) state)
     64)))))

(answer-part2 "AoC 2017" 256)

(answer-part2 (first (utils/read-input "2017/day10.txt")) 256)

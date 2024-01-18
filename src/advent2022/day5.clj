(ns advent2022.day5
  (:require [advent2022.utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]))
(use 'clojure.tools.trace)

(def lines (utils/read-resource-lines "input/day5.txt"))
(last lines)

(def parsed-lines
  (->> lines
       (partition-by string/blank?)
       (remove (fn [l] (= (nth l 0) "")))))

(def blocks (butlast (first parsed-lines)))
(def instructions (second parsed-lines))

(defn get-blocks
  ([str] (get-blocks str 0))
  ([str startIdx]
   (let [idx (.indexOf str "[" startIdx)]
     (if (= idx -1) '()
         (cons
          [(.charAt str (inc idx)) (inc idx)] (get-blocks str (inc idx)))))))

(mapcat get-blocks blocks)

(defn sort-piles [piles item]
  (let [[ch idx] item]
    (if (contains? piles idx)
      (assoc piles idx (conj (piles idx) ch))
      (assoc piles idx [ch]))))

(def initial-pile
  (let [pile (reduce sort-piles {} (mapcat get-blocks blocks))
        keymap (into {} (map-indexed (fn [n x] {x (inc n)}) (sort (keys pile))))]
    (set/rename-keys pile keymap)))
initial-pile

(def parsed-instructions
  (map #(->> %
           (re-matches #"move (\d+) from (\d+) to (\d+)")
           (rest)
           (map utils/parse-int))
     instructions))

(defn process-instruction
  [piles num pile dest-pile]
  (let [removed (take num (piles pile))]
    (assoc piles pile (nthrest (piles pile) num)
           dest-pile (concat (reverse removed) (piles dest-pile)))))

(defn process-instructions
  [piles instructions]
  (if (empty? instructions) piles
      (let [[num pile dest-pile] (first instructions)]
        (process-instructions (process-instruction piles num pile dest-pile)
                              (rest instructions)))))

;; answer to part 1
;; except it is wrong



(def processed-pile (process-instructions initial-pile parsed-instructions))

;; answer to part 1
(string/join
(map
 (fn [n] (first (processed-pile (inc n))))
 (range (count processed-pile))))

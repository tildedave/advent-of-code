(ns advent2022.day5
  (:require [utils :as utils]
            [clojure.string :as string]
            [clojure.set :as set]))

(def lines (utils/read-resource-lines "input/day5.txt"))


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
  [piles num pile dest-pile part-two]
  (let [removed (take num (piles pile))
        to-add (if part-two removed (reverse removed))]
    (assoc piles pile (nthrest (piles pile) num)
           dest-pile (concat to-add (piles dest-pile)))))

(defn process-instructions
  [piles instructions part-two]
  (if (empty? instructions) piles
      (let [[num pile dest-pile] (first instructions)]
        (process-instructions (process-instruction piles num pile dest-pile part-two)
                              (rest instructions)
                              part-two))))


;; answer to part 1
(let [processed-pile (process-instructions initial-pile parsed-instructions false)]
  (string/join
   (map
    (fn [n] (first (processed-pile (inc n))))
    (range (count processed-pile)))))

;; answer to part 2
(let [processed-pile (process-instructions initial-pile parsed-instructions true)]
  (string/join
   (map
    (fn [n] (first (processed-pile (inc n))))
    (range (count processed-pile)))))

(ns advent2018.day14
  (:require [utils :as utils]
            [clojure.string :as string]))

;; we don't have to do a linked list which is very kind
;; just vector and appending looks like it will work.

(def initial-state {:recipes [3 7] :elf1 0 :elf2 1})

(defn step [{:keys [recipes elf1 elf2]}]
  (let [current1 (get recipes elf1)
        current2 (get recipes elf2)
        sum-of-current (+ current1 current2)
        next-recipes (if (< sum-of-current 10)
                       (conj recipes sum-of-current)
                       (let [digit1 (quot sum-of-current 10)
                             digit2 (mod sum-of-current 10)]
                         (-> recipes (conj digit1) (conj digit2))))]
    {:recipes next-recipes
     :elf1 (mod (+ elf1 (inc current1)) (count next-recipes))
     :elf2 (mod (+ elf2 (inc current2)) (count next-recipes))}))

(defn answer [n]
  (->> (iterate step initial-state)
       (drop-while (fn [{:keys [recipes]}] (< (count recipes) (+ n 10))))
       (first)
       (#(subvec (:recipes %) n (+ n 10)))
       (string/join)
       ))

(assert (answer 9) "5158916779")
(assert (answer 5) "0124515891")
(assert (answer 18) "9251071085")
(assert (answer 2018) "5941429882")

(answer (->> (utils/read-input "2018/day14.txt") (first) (utils/parse-int)))

;; not clear why java Collections doesn't work for non-hardcoded seqs
;; https://stackoverflow.com/a/15224865/576087
(defn naive-sublist
  [sq sub]
  (->>
   (partition (count sub) 1 sq)
   (map-indexed vector)
   (filter #(= (second %) sub))
   (map first)))

(defn answer-part2 [s]
  (naive-sublist
   (:recipes (nth (iterate step initial-state) 50000000))
   (map #(utils/parse-int (str %)) (seq s))))

(answer-part2 (first (utils/read-input "2018/day14.txt")))

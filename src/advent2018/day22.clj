(ns advent2018.day22
  (:require [clojure.core.match :refer [match]]))

;; so this is a series of mutually recursive functions
;; it would be wonderful to memoize them.
;; we could do it with explicit state also (e.g. DP)
;; DP seems kind of simplest.

(defn region-type [depth target]
  ;; would be easier with explicit monads
  (letfn [(geologic-index
            [state [x y]]
           (if (contains? (:geologic-index state) [x y])
             [state (get-in state [:geologic-index [x y]])]
             (let [[state ans] (match [[x y]]
              [[0 0]] [state 0]
              [target] [state 0]
              [[x 0]] [state (* x 16807)]
              [[0 y]] [state (* y 48271)]
              [_] (let [[state el1] (erosion-level state [(dec x) y])
                        [state el2] (erosion-level state [x (dec y)])]
                    [state (* el1 el2)]))]
               [(assoc-in state [:geologic-index [x y]] ans)
                ans])))
          (erosion-level
            [state [x y]]
           (if (contains? (:erosion-level state) [x y])
             [state (get-in state [:erosion-level [x y]])]
             (let [[state geo-index] (geologic-index state [x y])
                   ans (mod (+ depth geo-index) 20183)]
               [(assoc-in state [:erosion-level [x y]] ans)
                ans])))]
    (fn [state [x y]]
      (let [[state el] (erosion-level state [x y])]
        [state (case (mod el 3)
                 0 :rocky
                 1 :wet
                 2 :narrow)]))))

((region-type 510 [10 10]) {} [1 1])

(defn total-risk-level [depth [tx ty]]
  (let [region-type (region-type depth [tx ty])]
    (second (reduce
     (fn [[state total-risk] coords]
       (let [[state region-type] (region-type state coords)]
         [state (+ total-risk (case region-type :rocky 0 :wet 1 :narrow 2))]
       ))
    [{} 0]
    (for [x (range 0 (inc tx))
          y (range 0 (inc ty))]
      [x y])))))

(total-risk-level 510 [10 10])
(total-risk-level 4002 [5 746])

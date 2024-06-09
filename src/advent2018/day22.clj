(ns advent2018.day22
  (:require [clojure.core.match :refer [match]]
            [grid :as grid]))

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
             (let [[state ans]
                   (match [[x y]]
                     [[0 0]] [state 0]
                     [target] [state 0]
                     [[x 0]] [state (* x 16807)]
                     [[0 y]] [state (* y 48271)]
                     :else (let [[state el1] (erosion-level state [(dec x) y])
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

(defn region-type! [depth target]
  (let [state-ref (atom {})
        region-type (region-type depth target)]
    (fn [[x y]]
      (let [[state ans] (region-type (deref state-ref) [x y])]
          (reset! state-ref state)
          ans))))

(defn total-risk-level [depth [tx ty]]
  (let [region-type! (region-type! depth [tx ty])]
    (reduce
     (fn [total-risk coords]
       (+ total-risk
          (case (region-type! coords) :rocky 0 :wet 1 :narrow 2)))
     0
    (for [x (range 0 (inc tx))
          y (range 0 (inc ty))]
      [x y]))))

(total-risk-level 510 [10 10])
;; these are my puzzle coords.
(total-risk-level 4002 [5 746])


;; part 2 is A* search.
;; The fastest route might involve entering regions beyond the X or Y coordinate of the target.
;; it feels like we need to explicitly keep our "computed" state in our nodes.
;; I think we can "atomize" our function.  yes that was simple.

;; A* search needs a neighbor function, a start node, and a "done" detector.

(defn neighbors [region-type!]
  (fn [state]
    (let [{:keys [position equipped]} state
          [px py] position]
      (->>
       (for [[dx dy] grid/cardinal-directions]
         (let [[nx ny] [(+ px dx) (+ py dy)]]
           (cond
             (< nx 0) nil
             (< ny 0) nil
             :else
             (match [equipped (region-type! [nx ny])]
               [:torch (:or :rocky :narrow)] (-> state
                                             (assoc :position [nx ny])
                                             (assoc :minutes 1))
               [:neither (:or :wet :narrow)] (-> state
                                             (assoc :position [nx ny])
                                             (assoc :minutes 1))
               [:climbing-gear (:or :rocky :wet)] (-> state
                                                  (assoc :position [nx ny])
                                                  (assoc :minutes 1))
               :else nil))))
       (remove nil?)
       (cons
        (let [next-equipped (match [equipped (region-type! [px py])]
                              [:torch :rocky] :climbing-gear
                              [:torch :narrow] :neither
                              [:climbing-gear :rocky] :torch
                              [:climbing-gear :wet] :neither
                              [:neither :narrow] :torch
                              [:neither :wet] :climbing-gear)]

          (-> state
              (assoc :equipped next-equipped)
              (assoc :minutes 7))))))))

(defn rescue-time [depth target]
  (let [region-type! (region-type! depth target)]
    (grid/a*-search
     {:position [0 0] :equipped :torch}
     (fn [{:keys [position equipped]}] (and (= position target)
                                            (= equipped :torch)))
     (neighbors region-type!)
     ;; just use the distance from target heuristic I guess
     (fn [{:keys [position]}]
       (let [[px py] position]
         (+ (abs (- px (first target))) (abs (- py (second target))))))
     ;; easiest to tag the neighbor with the minutes to travel to
     (fn [_ {:keys [minutes]}] minutes)
     (fn [state] (dissoc state :minutes))
     )))

(rescue-time 510 [10 10])
(rescue-time 4002 [5 746])

(ns advent2018.day9
  (:require [utils :as utils]))

(defrecord Node [left num right])

(defn initial-node []
  (let [initial (atom (Node. (atom nil) 0 (atom nil)))]
    (swap! initial #(-> %
                        (assoc :left initial)
                        (assoc :right initial)))
    initial))

(defn insert-after!
  "Inserts a number after a node and returns a node containing that number."
  [node ^Integer num]
  (let [new (atom (Node. node
                         num
                         (:right (deref node))))]
    (swap! (:right (deref node)) #(assoc % :left new))
    (swap! node #(assoc % :right new))
    new))

(defn sanity-check [node]
  (assert (= (:right (deref (:left (deref node)))) node))
  (assert (= (:left (deref (:right (deref node)))) node)))

(sanity-check (initial-node))

(defn sanity-check-all [node]
  (loop [node node
         seen #{}]
    (if (contains? seen node)
      nil
      (do
        (sanity-check node)
        (recur (:right (deref node))
               (conj seen node))))))

(defn remove-node!
  "Removes a node and returns the node immediately clockwise"
  [node]
  (let [old-left (:left (deref node))
        old-right (:right (deref node))]
    (swap! old-left #(assoc % :right old-right))
    (swap! old-right #(assoc % :left old-left))
    old-right))

(defn walk-n
  [node n]
  (cond
    (zero? n) node
    (< n 0) (recur (:left (deref node)) (inc n))
    (> n 0) (recur (:right (deref node)) (dec n))))

(:num
 (deref
  (walk-n
   (reduce
    insert-after!
    (initial-node)
    (range 1 10))
   4)))

;; (:num (deref (insert-after! (initial-node) 1)))

;; (:num (deref (:left (initial-node))))

(defn nums [node]
  (loop [node node
         seen #{}
         result []]
    (if (contains? seen node)
      result
      (recur
       (:right (deref node))
       (conj seen node)
       (conj result (:num (deref node)))))))

(sanity-check (reduce
       (fn [node n] (insert-after! node n))
       (initial-node)
       (range 1 3)))

(defn nums-left [node]
  (loop [node node
         seen #{}
         result []]
    (if (contains? seen node)
      result
      (recur
       (:left (deref node))
       (conj seen node)
       (conj result (:num (deref node)))))))

(nums (initial-node))

(defn parse-input [line]
  (->> line
       (re-matches #"(\d+) players; last marble is worth (\d+) points")
       (rest)
       (map utils/parse-int)))

(->> (utils/read-input "2018/day9.txt")
     (first)
     (parse-input))

(defn step [state]
  (let [{:keys [current-player
                current-marble
                next-marble
                num-players]} state
        next-player (mod (inc current-player) num-players)]
    (if
     (zero? (mod next-marble 23))
      (let [marble (walk-n current-marble -7)
            remove-marble (:num (deref marble))]
        (-> state
            (assoc :current-player next-player)
            (update :next-marble inc)
            (update-in [:scores current-player] (fnil (partial + next-marble remove-marble) 0))
            (assoc :current-marble (remove-node! marble))))
      (-> state
          (assoc :current-player next-player)
          (update :next-marble inc)
          (assoc :current-marble (insert-after! (walk-n current-marble 1) next-marble))))))

(defn answer [num-players last-marble]
  (->> (nth (iterate step {:current-player 0
                           :next-marble 1
                           :current-marble (initial-node)
                           :num-players num-players})
            last-marble)
       :scores
       (vals)
       (reduce max)))

(defn score-turns [num-players last-marble]
  (->> (take
        (inc last-marble)
        (iterate step {:current-player 0
                       :next-marble 1
                       :circle [0]
                       :num-players num-players}))
       (filter #(:scored %))
       (map :scored)))

;; (score-turns 10 1618)

(answer 9 25)
(answer 10 1618)
(answer 13 7999)
(answer 17 1104)
(answer 21 6111)
(answer 30 5807)

(->> (utils/read-input "2018/day9.txt")
     (first)
     (parse-input)
     (apply answer)
     (time))

(->> (utils/read-input "2018/day9.txt")
     (first)
     (parse-input)
     (map-indexed (fn [n x] (if (zero? n) x (* x 100))))
     (apply answer))

(answer 427 (* 100 70723))

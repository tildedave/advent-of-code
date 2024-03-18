(ns advent2020.day15
  (:require [utils :as utils]))

;; what do we have to store?
;; for each number, the last turn that it came at.
;; and we have to remember the number that was last

(defn next-number [turn-num spoken-last starting-numbers number-history]
  (if-let [x (first starting-numbers)]
    x
    (let [last-spoken (number-history spoken-last)]
      (cond
        (nil? last-spoken) 0
        (= (count last-spoken) 1) 0
        :else (- (dec turn-num) (second last-spoken))))))

(defn step [[turn-num spoken-last starting-numbers number-history]]
  (let [next-num (next-number turn-num spoken-last starting-numbers number-history)]
    [(inc turn-num)
     next-num
     (rest starting-numbers)
     (update number-history next-num (fnil #(->> % (cons turn-num) (take 2)) '()))]))

(defn game-sequence [starting-numbers]
  (iterate step [1 -1 starting-numbers {}]))

(second (nth (game-sequence [0 3 6]) 2020))

(defn solve-direct [starting-numbers stop-idx]
  (let [starting-history (->> starting-numbers
                              (map-indexed vector)
                              (map #(update % 0 inc))
                              (map (fn [[n x]] [x n]))
                              (into {}))]
  (loop
   [i (inc (count starting-numbers))
    number-history starting-history
    say-next 0]
    (cond
      (= i stop-idx) say-next
      (contains? number-history say-next)
        (recur (inc i)
               (assoc number-history say-next i)
               (- i (number-history say-next)))
      :else
        (recur
         (inc i)
         (assoc number-history say-next i)
         0)))))

(defn answer-part1-direct [starting-numbers]
  (solve-direct starting-numbers 2020))

(defn answer-part1 [starting-numbers]
  (-> starting-numbers
      (game-sequence)
      (nth 2020)
      (second)))

(defn parse-input [filename]
  (map utils/parse-int
       (-> (format "2020/%s" filename)
           (utils/read-input)
           (first)
           (.split ","))))



(parse-input "day15.txt")

(answer-part1 [0 3 6])
(answer-part1 (parse-input "day15.txt"))
(answer-part1-direct (parse-input "day15.txt"))

(defn number-sequence [game-sequence]
  (map second game-sequence))


;; let's just brute force it.  I've got all day.
(time (solve-direct (parse-input "day15.txt") 30000000))
(println (time (nth (number-sequence (game-sequence (parse-input "day15.txt"))) 30000000)))


(->> (number-sequence (game-sequence [0 3 6]))
     (map-indexed vector)
     (filter #(= (second %) 0)))

(- 3719 3286)
(- 3719 3286)

(answer-part1 [0 3 6])
(answer-part1 (parse-input "day15.txt"))

;; (speak-number {1 0 2 3 3 6} {0 [1] 3 [2] 6 [3]} 4)

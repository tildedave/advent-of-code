(ns advent2020.day15
  (:require [utils :as utils]))

(defn next-number [turn-num starting-numbers turn-history number-history]
  (if-let [x (first starting-numbers)]
    x
    (let [spoken-last (get turn-history (dec turn-num))
          last-spoken (number-history spoken-last)]
      (case (count last-spoken)
                   ;; I believe this is impossible as we should have set it
                   ;; in the turn history
        0 (throw (Exception. "should be impossible"))
        1 0
        (- (first last-spoken) (second last-spoken))))))

(defn step [[turn-num starting-numbers turn-history number-history]]
  (let [next-num (next-number turn-num starting-numbers turn-history number-history)]
    [(inc turn-num)
     (rest starting-numbers)
     (assoc turn-history turn-num next-num)
     (update number-history next-num (fnil #(->> % (cons turn-num) (take 2)) '()))]))

(defn game-sequence [starting-numbers]
  (iterate step [1 starting-numbers {} {}]))

(defn answer-part1 [starting-numbers]
  ((-> (nth (game-sequence starting-numbers) 2020) (nth 2)) 2020))

(defn parse-input [filename]
  (map utils/parse-int
       (-> (format "2020/%s" filename)
           (utils/read-input)
           (first)
           (.split ","))))

(defn number-sequence [game-sequence]
  (map-indexed
   (fn [idx [_ _ turn-history]] (turn-history idx))
   game-sequence))


;; let's just brute force it.  I've got all day.
(println (time (nth (number-sequence (game-sequence (parse-input "day15.txt"))) 30000000)))

(->> (number-sequence (game-sequence [0 3 6]))
     (map-indexed vector)
     (filter #(= (second %) 0)))

(- 3719 3286)
(- 3719 3286)

(answer-part1 [0 3 6])
(answer-part1 (parse-input "day15.txt"))

;; (speak-number {1 0 2 3 3 6} {0 [1] 3 [2] 6 [3]} 4)

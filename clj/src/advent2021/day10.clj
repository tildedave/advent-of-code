(ns advent2021.day10
  (:require [utils :as utils]))

(def score-map {\) 3
                \] 57
                \} 1197
                \> 25137})

(def starts #{\( \[ \{ \<})
(def ends {\( \) \[ \] \{ \} \< \>})
(type (pop (list 1 2 3)))

(.charAt "Abc" 0)
(seq "abc")
(defn parse-chunks [str]
  (loop [ch-seq (seq str)
         stack (list)]
    (if-let [ch (first ch-seq)]
      (cond
        (contains? starts ch)
        (recur (rest ch-seq) (conj stack ch))
        (= ch (ends (peek stack)))
        (recur (rest ch-seq) (pop stack))
          ;; boom
        :else {:error ch})
      {:incomplete stack})))

(parse-chunks "{([(<{}[<>[]}>{[]{[(<()>")
(parse-chunks "[({(<(())[]>[[{[]{<()<>>")

(defn part1-score [str]
  (let [result (parse-chunks str)]
    (if (result :error)
      (score-map (result :error))
      0)))

(defn answer-part1 [lines]
  (->> lines
       (map part1-score)
       (reduce +)))

(answer-part1 (utils/read-resource-lines "input/day10-example.txt"))
(answer-part1 (utils/read-resource-lines "input/day10.txt"))

(def part2-map {\) 1 \] 2 \} 3 \> 4})

(defn part2-score [str]
  (let [result (parse-chunks str)]
    (if (result :incomplete)
      (reduce
       (fn [score ch]
         (+ (* score 5) (part2-map (ends ch))))
       0
       (result :incomplete))
      0)))

(part2-score "<{([{{}}[<[[[<>{}]]]>[]]")

(defn answer-part2 [lines]
  (let [l (->> lines
               (map part2-score)
               (remove zero?)
               (sort))]
    (nth l (quot (count l) 2))))

(answer-part2 (utils/read-resource-lines "input/day10-example.txt"))
(answer-part2 (utils/read-resource-lines "input/day10.txt"))

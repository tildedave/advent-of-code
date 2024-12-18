(ns advent2024.day18
  (:require
    [grid :as grid]
    [utils :as utils]
    [graph :as graph]))

;; A* search
;; I would expect that the bits are going to start falling in real-time in part 2

(def example-grid (grid/make 7 7 \.))
(def example-coords (utils/read-input "2024/day18-example.txt"))
(defn parse-coords [lines] (map #(vec (utils/str->nums %)) lines))
(parse-coords example-coords)
(defn coords-fall [grid coords-list]
  (reduce (fn [grid coords] (grid/assoc grid coords \O)) grid coords-list))

(let [example (coords-fall example-grid (take 12 (parse-coords example-coords)))]
  (graph/a*-search
   [0 0]
   #(= % [6 6])
   (fn [coords] (grid/neighbors example coords grid/cardinal-directions #(= % \O)))
   (fn [_] 10)
   (fn [_ _] 1)))

(defn answer-part1 [start end falling-coords]
(let [grid (coords-fall (grid/make (inc (first end)) (inc (second end)) \.) falling-coords)]
  (first
   (graph/a*-search
   start
   #(= % end)
   (fn [coords] (grid/neighbors grid coords grid/cardinal-directions #(= % \O)))
   (fn [_] 10)
   (fn [_ _] 1)))))

(answer-part1 [0 0] [6 6] (take 12 (parse-coords example-coords)))
(answer-part1 [0 0] [70 70] (take 1024 (parse-coords (utils/read-input "2024/day18.txt"))))

(answer-part1 [0 0] [6 6] (take 20 (parse-coords example-coords)))


;; first byte that cuts off the path to the exit
;; uh, isn't this very easy?  yes, looks like it.

(answer-part1 [0 0] [70 70] (take 2048 (parse-coords (utils/read-input "2024/day18.txt"))))

(defn answer-part2 [start end falling-coords]
  (loop
   [lo 0  ;; YES, still reachable
    hi (count falling-coords)]
    ;; (println lo hi)
    (if (= (inc lo) hi)
      (first (drop lo falling-coords))
      (let [x (quot (+ lo hi) 2)
            reachable? (try
                         (do
                           (answer-part1 start end (take x falling-coords))
                           true)
                         (catch Exception _ false))]
        (if reachable?
          (recur x hi)
          (recur lo x))))))

(answer-part2 [0 0] [6 6] (parse-coords example-coords))
(answer-part2 [0 0] [70 70] (parse-coords (utils/read-input "2024/day18.txt")))

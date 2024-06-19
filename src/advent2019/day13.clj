(ns advent2019.day13
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]
            [clojure.core.match :refer [match]]))

;; part 1
(let [input (a/chan)
      output (intcode/run-file "2019/day13.txt" input)]
  (->>
   (<!! (a/go-loop [game-board {}]
          (let [x (<! output)
                y (<! output)
                tile-id (<! output)]
            (if (nil? x)
              (do
                (a/close! input)
                game-board)
              (recur
               (assoc game-board [x y] tile-id))))))
   (vals)
   (filter (partial = 2))
   (count)))

(defn desired-input [[bx _] [px _]]
  (compare bx px))

;; part 2
(let [input (a/chan)
      program (-> (intcode/parse-file "2019/day13.txt")
                  (assoc-in [:program 0] 2))
      output (intcode/run-program program input)]
  (println
   (<!! (a/go-loop [current-score 0
                    ball-position nil
                    paddle-position nil]
          (let [[v ch] (a/alts!
                        (if (and ball-position paddle-position)
                          [output [input (desired-input ball-position paddle-position)]]
                          [output]))]
            (if (= ch output)
              (match [v (<! output) (<! output)]
                [nil nil nil] (do
                                (a/close! input)
                                current-score)
                ;; "recur can only be used in tail position" <- wrong, you are
                ;; in tail position.
                [-1 0 new-score] (recur new-score ball-position paddle-position)
                [bx by 4] (recur current-score [bx by] paddle-position)
                [px py 3] (recur current-score ball-position [px py])
                [_ _ _] (recur current-score ball-position paddle-position))
              ;; otherwise whatever
              (recur current-score ball-position paddle-position)))))))

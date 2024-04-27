(ns advent2016.day12
  (:require [utils :as utils]))

(def ^:dynamic part2? false)

(defn parse-instruction [s]
  (let [[s1 s2 s3] (.split s " ")]
    (case s1
      "cpy" [:copy (utils/try-parse-int s2) s3]
      "inc" [:inc s2]
      "dec" [:dec s2]
      "jnz" [:jnz (utils/try-parse-int s2) (utils/parse-int s3)])))

(defn exec [program]
  (fn [[n state]]
    (if-let [[instr x y] (get program n)]
      (case instr
        :copy [(inc n)
               (if (number? x)
                 (assoc state y x)
                 (assoc state y (state x)))]
        :inc [(inc n) (update state x inc)]
        :dec [(inc n) (update state x dec)]
        :jnz (if (zero? (if (number? x) x (state x)))
               [(inc n) state]
               [(+ n y) state]))
      [n state])))

(defn program-seq [filename]
  (iterate (exec (->> (utils/read-input filename)
                      (mapv parse-instruction)))
           [0
            {"a" 0 "b" 0 "c" (if part2? 1 0) "d" 0}]))

(program-seq "2016/day12-example.txt")


(->> (utils/read-input "2016/day12.txt")
     (mapv parse-instruction))

(defn answer [filename]
  (let [program (->> (utils/read-input filename)
                     (mapv parse-instruction))]
    (reduce (fn [_ [n state]]
              (if (get program n) nil (reduced (state "a"))))
            (program-seq filename))))

;; (drop 30000000 (map (fn [[_ n state]] [n state]) (program-seq "2016/day12.txt")))

(answer "2016/day12-example.txt")
(answer "2016/day12.txt")

(binding [part2? true] (answer "2016/day12.txt"))

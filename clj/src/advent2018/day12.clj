(ns advent2018.day12
  (:require [clojure.string :as string]
            [utils :as utils]))

(defn parse-initial-state [^String line]
  (->> (.split line "initial state: ")
       (second)
       (map-indexed vector)
       (reduce (fn [acc [n x]] (if (= x \#) (merge acc {n x}) acc)) {})))

(defn parse-transformation [^String line]
  (let [[lhs rhs] (.split line " => ")]
    {lhs (.charAt rhs 0)}))

(parse-initial-state "initial state: #..#.#..##......###...###")
(parse-transformation ".#.## => #")

(range -2 2)

(defn step [transformations]
  (fn [pots]
    (let [min-x (reduce min (keys pots))
          max-x (reduce max (keys pots))]
      (reduce
       (fn [next-pots idx]
         (let [hash (string/join (map #(get pots % \.) (range (- idx 2) (+ idx 3))))
               result (get transformations hash \.)]
           (if (= result \#)
             (assoc next-pots idx result)
             (dissoc next-pots idx))))
       pots
       (range (- min-x 2) (+ max-x 2))))))

(defn print-pots [pots]
  (let [min-x (reduce min (keys pots))
        max-x (reduce max (keys pots))]
    (string/join (map #(get pots % \.) (range min-x (inc max-x))))))

(let [[pots transformations] (->> (utils/read-input "2018/day12-example.txt")
                                  (utils/split-by "")
                                  ((fn [[[initial-str] trans-strs]]
                                     [(parse-initial-state initial-str)
                                      (reduce merge (map parse-transformation trans-strs))])))]
  (map print-pots (iterate (step transformations) pots)))

(defn answer [filename]
  (let [[pots transformations] (->> (utils/read-input filename)
                                    (utils/split-by "")
                                    ((fn [[[initial-str] trans-strs]]
                                       [(parse-initial-state initial-str)
                                        (reduce merge (map parse-transformation trans-strs))])))]
    (->> (nth (iterate (step transformations) pots) 20)
         (keys)
         (reduce +))))

(println (answer "2018/day12-example.txt"))
(println (answer "2018/day12.txt"))

(defn pot-sum [pots]
  (reduce + (keys pots)))

(defn answer-part2 [filename]
  (let [[pots transformations] (->> (utils/read-input filename)
                                    (utils/split-by "")
                                    ((fn [[[initial-str] trans-strs]]
                                       [(parse-initial-state initial-str)
                                        (reduce merge (map parse-transformation trans-strs))])))
        pot-sum-seq (->> (iterate (step transformations) pots)
                         (map pot-sum))
        diff-seq (map (fn [n x1 x2] [n (- x1 x2)]) (range) (rest pot-sum-seq) pot-sum-seq)
        [n delta] (->> (map vector diff-seq (rest diff-seq))
                       (drop-while (fn [[[_ diff1] [_ diff2]]] (not= diff1 diff2)))
                       (first)
                       (first))]
    (+ (nth pot-sum-seq n) (* delta (- 50000000000N n)))))

(* (- 50000000000 128) 78)

(println (answer-part2 "2018/day12.txt"))

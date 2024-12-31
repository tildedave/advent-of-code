(ns advent2017.day13
  (:require [utils :as utils]))

(defn parse-line [s]
  (let [[n length] (->> (.split ^String s ": ")
                        (map utils/parse-int))]
    {n {:position 0, :length length,
        :direction 1}}))

(defn parse-lengths [filename]
  (->> (utils/read-input filename)
       (map parse-line)
       (reduce merge {})))

(parse-lengths "2017/day13-example.txt")

(defn tick [state]
  (update-vals
   state
   (fn [r]
     (let [{:keys [position length direction]} r]
       (let [reverse-direction? (or
                                 (and (= direction -1) (= position 0))
                                 (and (= direction 1) (= position (dec length))))]
         (if reverse-direction?
           (-> r
               (assoc :position (+ position (- direction)))
               (assoc :direction (- direction)))
           (update r :position #(+ % direction))))))))

(defn answer [filename]
  (let [state (parse-lengths filename)
        n (reduce max (keys state))]
    (->>
     (iterate tick state)
     (take (inc n))
     (reduce
     (fn [[my-position total-severity] state]
       (if-let [{:keys [length position]} (state my-position)]
         [(inc my-position)
          (if (zero? position)
            (+ total-severity (* length my-position))
            total-severity)]
         [(inc my-position)
          total-severity]))
     [0 0]
     )
     (second))))

(nth (iterate tick (parse-lengths "2017/day13-example.txt")) 4)

(answer "2017/day13-example.txt")
(answer "2017/day13.txt")

(defn is-caught? [state-seq]
  (let [n (reduce max (keys (first state-seq)))]
    (->>
     state-seq
     (take (inc n))
     (reduce (fn [[my-position caught?] state]
               (if (and (contains? state my-position)
                        (zero? (get-in state [my-position :position])))
                   (reduced [my-position true])
                   [(inc my-position) false]))
             [0 false])
     (second))))

(is-caught? (drop 9 (iterate tick (parse-lengths "2017/day13-example.txt"))))

;; brute force for part 2 😎
(let [lengths (parse-lengths "2017/day13.txt")]
  (reduce
   (fn [state-seq n]
     (if (is-caught? state-seq)
       (rest state-seq)
       (reduced n)))
   (iterate tick lengths)
   (range)))

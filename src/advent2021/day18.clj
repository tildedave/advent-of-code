(ns advent2021.day18
    (:require [advent2021.utils :as utils]))

;; snailfish numbers are nested vectors
;; we want to find the action to do, and then do it.
;;
;; If any pair is nested inside four pairs, the leftmost such
;; pair explodes.
;; If any regular number is 10 or greater, the leftmost such
;; regular number splits.
;; so we walk the vector.
;;
;; it feels like treating this as a vector makes this harder.
;; this is a big ol goto loop

;; paths in the number are like, 0 1 1 0 1 etc.
;; never a 2.

(def x [[[[[9 8] 1] 2] 3] 4])
(get-in x [0 0 0 0 0])
(get-in x [0 1])
(get-in x [0])

(defn next-path [num path]
  (loop [path path]
    (if (empty? path) nil
        (let [curr (get-in num path)
              end-path (peek path)]
          (cond
            (vector? curr) (conj path 0)
            (= end-path 0) (conj (pop path) 1)
      ;; we've exhausted the current pair and need to go to the next of the
      ;; current pair that we're in.  but if we're at a 1 for the current pair.
      ;; this involves a pop, and it can involve going up multiple levels.
      ;; so this is somehow recursive.
      ;; e.g. [0 0 1] -> recurse on [0 1].
      ;; [0 1 1] -> recurse on [1]
      ;; strip from end of the list until we hit a 0, increment that, recurse.
            (= end-path 1)
            (loop [path (pop path)]
              (cond (empty? path)
                    nil
                    (= (peek path) 0)
                    (conj (pop path) 1)
                    :else (recur (pop path))))
            :else (throw (Exception. "invalid logic (next-path)")))))))

;; (def x [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]])
(->> (iterate #(next-path x %) [0])
     (partition-by nil?)
     (first)
     (map #(get-in x %))
     (filter #(not (vector? %))))

(defn prev-path [num path]
  (if (empty? path) nil
      (let [curr (get-in num path)
            end-path (peek path)]
        (cond
          (= end-path 0) (pop path)
          (= end-path 1) (conj (pop path) 0)
          :else (throw (Exception. "invalid logic (prev-path)"))))))

(defn next-paths [num]
  (->> (iterate #(next-path num %) [0])
       (partition-by nil?)
       (first)))

(defn prev-num [num path]
  (->> (iterate #(prev-path num %) path)
       (rest)
       ;; annoyingly we have to get rid of the nils.
       (partition-by nil?)
       (first)
       ;; first number in the sequence.
       (map #(get-in num %))
       (filter number?)
       (first)))

(assoc-in [1 2 3] [2] 4)

(defn follow-path [num f path]
  (loop [path (f num path)]
    (cond
      (nil? path) nil
      (number? (get-in num path)) path
      :else
      (recur (f num path)))))

(defn reduce-snailfish-num-step [num]
  (loop [path [0]]
    (if (nil? path)
      [num false]
    (let [curr (get-in num path)]
     (cond
       (number? curr)
       (if (>= curr 10)
         ;; split the number.
         [(assoc-in num path [(quot curr 2) (utils/quot-round-up curr 2)]) true]
         (recur (next-path num path)))
       (vector? num)
       (if (>= (count path) 4)
         ;; we explode this one.
         ;; we have to find the left/right paths that are numbers,
         ;; update the number by adding x / y, then replace current path
         ;; with 0.
         ;; so this just looks like a few assoc-in statements.
         ;; but they are conditional.
         (let [[x y] curr
               left-path (follow-path num prev-path path)
               ;; we have to "next-path" ourselves out of the current vector
               right-path (follow-path num next-path (conj path 1))]
           [(->> num
                 (#(if (nil? left-path) % (update-in % left-path (partial + x))))
                 (#(if (nil? right-path) % (update-in % right-path (partial + y))))
                 (#(assoc-in % path 0)))
            true])
         (recur (next-path num path))))))))

(get-in x [0 0 0 0])

(def y [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]])

(follow-path y next-path [0 1 1 1 1])

(defn reduce-snailfish-num [num]
  (loop [num num]
    (let [[next-num reduced] (reduce-snailfish-num-step num)]
      (if reduced (recur next-num)
          next-num))))

(defn add-snailfish-num [num1 num2]
  (reduce-snailfish-num [num1 num2]))

(add-snailfish-num
 [[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]]
 [7 [[[3 7] [4 3]] [[6 3] [8 8]]]])

(reduce-snailfish-num [[[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1]])

(reduce-snailfish-num-step [7 [6 [5 [4 [3 2]]]]])

(reduce-snailfish-num [7 [6 [5 [4 [3 2]]]]])

(update-in [[[[[9 8] 1] 2] 3] 4] [0 0 0 0 0] inc)

       (vector? num) (let [[x y] num]
                       (if (= depth 4) ;; we need to explode f - in the current result.

                         (if-let [result (reduce-snailfish-num f (inc depth))]
          ;; if the result has changed  we need to return the current result.
                           (into [result] (subvec num 1))
                           (if-let [result (reduce-snailfish-num (subvec num 1) depth)]
                             (into [f] [result])
                             nil)))))))

  (reduce-snailfish-num [[[[[9 8] 1] 2] 3] 4])

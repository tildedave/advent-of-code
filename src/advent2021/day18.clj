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

;; an incredibly stupid hack here would be to start from 0 and
;; iterate until next-path is the path we want.
;; let's do the stupid hack.
(defn prev-path-too-smart [num path]
  (if (empty? path) nil
      (let [curr (get-in num path)
            end-path (peek path)]
        (cond
          (= end-path 0) (pop path)
          (= end-path 1) (conj (pop path) 0)
          :else (throw (Exception. "invalid logic (prev-path)"))))))

(defn prev-path [num orig-path]
  (loop [path [0]]
    (if (nil? path) nil
    (let [np (next-path num path)]
      (if (= np orig-path) path
          (recur np))))))

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

(def q [[[[4 0] [5 0]] [[[4 5] [2 6]] [9 5]]] [7 [[[3 7] [4 3]] [[6 3] [8 8]]]]])

(prev-path q [0])
(prev-path q [0 0 1])
(get-in q [0 0 1])

(map
 vector
 (next-paths q)
 (rest (map #(prev-path q %) (next-paths q))))


(defn follow-path [num f path]
  (loop [path (f num path)]
    (cond
      (nil? path) nil
      (number? (get-in num path)) path
      :else
      (recur (f num path)))))


;; OK so the problem is that we need to check ALL for splits.
;; then we check ALL for explodes.

(defn reduce-snailfish-num-explodes [num]
  (loop
   [path [0]]
    (if
     (nil? path)
      [num false]
      (let [curr (get-in num path)]
        (if (and (vector? curr) (>= (count path) 4))
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
                 ;; question: does this invalidate the paths?
                  (#(if (nil? left-path) % (update-in % left-path (partial + x))))
                  (#(if (nil? right-path) % (update-in % right-path (partial + y))))
                  (#(assoc-in % path 0)))
             [:explode curr]])
          (recur (next-path num path)))))))

(defn reduce-snailfish-num-splits [num]
  (loop
   [path [0]]
    (if
     (nil? path)
      [num false]
      (let [curr (get-in num path)]
        (if (and (number? curr) (>= curr 10))
          [(assoc-in num path [(quot curr 2) (utils/quot-round-up curr 2)])
           [:split curr]]
          (recur (next-path num path)))))))

(defn reduce-snailfish-num-step [num]
  ;; first try to explode, then try to split.
  (let [[next-num result] (reduce-snailfish-num-explodes num)]
    (if result [next-num result]
        (reduce-snailfish-num-splits next-num))))

(def explode-examples
  [
  {:input [[[[[9,8],1],2],3],4] :expected [[[[0,9],2],3],4]}
  {:input [7,[6,[5,[4,[3,2]]]]] :expected [7,[6,[5,[7,0]]]]}
  {:input [[6,[5,[4,[3,2]]]],1] :expected [[6,[5,[7,0]]],3]}
  {:input [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
    :expected [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]}
  {:input [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
    :expected [[3,[2,[8,0]]],[9,[5,[7,0]]]]}
  ])

(doseq [example explode-examples]
  (let [{:keys [input expected]} example]
    (assert (= (first (reduce-snailfish-num-step input)) expected))))

(reduce-snailfish-num-step [[[[[9 8] 1] 2] 3] 4])
(= (reduce-snailfish-num-step [[[[[9 8] 1] 2] 3] 4])
   [[[[[0 9] 2] 3] 4] [:explode [9 8]]])

(defn reduce-snailfish-num [num]
  (loop [num num]
    (let [[next-num reduced] (reduce-snailfish-num-step num)]
      (if reduced (recur next-num)
          next-num))))

(defn add-snailfish-num [num1 num2]
  (reduce-snailfish-num [num1 num2]))

;; so this is wrong
(add-snailfish-num
 [[[0 [4 5]] [0 0]] [[[4 5] [2 6]] [9 5]]]
 [7 [[[3 7] [4 3]] [[6 3] [8 8]]]])

(def add-list-examples
  [{:input [[1,1]
            [2,2]
            [3,3]
            [4,4]]
     :expected [[[[1,1],[2,2]],[3,3]],[4,4]]}
   {:input [[1,1]
             [2,2]
             [3,3]
             [4,4]
             [5,5]]
     :expected [[[[3,0],[5,3]],[4,4]],[5,5]]}
   {:input [[1,1]
             [2,2]
             [3,3]
             [4,4]
             [5,5]
             [6,6]]
     :expected [[[[5,0],[7,4]],[5,5]],[6,6]]}])

(for [example add-list-examples]
  (let [{:keys [input expected]} example]
    (assert (= (reduce add-snailfish-num input) expected))))

(add-snailfish-num [[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                   [7,[[[3,7],[4,3]],[[6,3],[8,8]]]])

(reduce add-snailfish-num
        [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
         [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
         [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
         [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
         [7,[5,[[3,8],[1,4]]]]
         [[2,[2,2]],[8,[8,1]]]
         [2,9]
         [1,[[[9,3],9],[[9,0],[0,7]]]]
         [[[5,[7,4]],7],1]
         [[[[4,2],2],6],[8,7]]])


(defn magnitude [num]
  (cond
    (vector? num)
    (+ (* 3 (magnitude (first num)))
       (* 2 (magnitude (second num))))
    (number? num)
    num))

(defn answer-part1 [lines]
  (->> lines
     (map read-string)
     (reduce add-snailfish-num)
     (magnitude)))

(answer-part1 (utils/read-input "day18-example.txt"))
(answer-part1 (utils/read-input "day18.txt"))

(reduce add-snailfish-num (map ))

(def z [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]] [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]])

(iterate #(first (reduce-snailfish-num-step %)) z)

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

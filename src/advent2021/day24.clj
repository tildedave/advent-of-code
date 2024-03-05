(ns advent2021.day24
  (:require [advent2021.utils :as utils]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(def instruction-res
  '(#"^(inp) (w|x|y|z)$"
    #"^(add) (w|x|y|z) (w|x|y|z|\-?\d+)$"
    #"^(mul) (w|x|y|z) (w|x|y|z|\-?\d+)$"
    #"^(div) (w|x|y|z) (w|x|y|z|\-?\d+)$"
    #"^(mod) (w|x|y|z) (w|x|y|z|\-?\d+)$"
    #"^(eql) (w|x|y|z) (w|x|y|z|\-?\d+)$"))

(map #(re-matches % "mul z 3") instruction-res)

(defn parse-line [line]
  (->> instruction-res
       (map #(re-matches % line))
       (remove nil?)
       (first)
       (rest)
       (mapv (fn [x] (if (number? (utils/try-parse-int x))
                       ['number (utils/parse-int x)]
                       x)))))

;; good enough
(defn instruction-eval [state prog]
  (let [num (fn [var-or-num] (match [var-or-num]
                               [['number n]] n
                               :else (state var-or-num)))]
    (match prog
      ["inp" v] (-> state
                    (assoc v (first (state :input)))
                    (update :input rest))
      ["add" v var-or-num] (-> state
                               (update v (partial + (num var-or-num))))
      ["mul" v var-or-num] (-> state
                               (update v (partial * (num var-or-num))))
      ["div" v var-or-num] (-> state
                               (update v #(quot % (num var-or-num))))
      ["mod" v var-or-num] (-> state
                               (update v #(mod % (num var-or-num))))
      ["eql" v var-or-num] (-> state
                               (update v #(if (= % (num var-or-num)) 1 0))))))

(defn initial-state [input]
  {"w" 0 "x" 0 "y" 0 "z" 0 :input input})

(defn run-program [input parsed-lines]
  (->> parsed-lines
       (reduce instruction-eval (initial-state input))))

(->> (list "inp x" "mul x -1")
     (map parse-line)
     (run-program (list 8)))

(->> (list
      "inp z"
      "inp x"
      "mul z 3"
      "eql z x")
     (map parse-line)
     (run-program (list 2 7)))

(->> (list
      "inp w"
      "add z w"
      "mod z 2"
      "div w 2"
      "add y w"
      "mod y 2"
      "div w 2"
      "add x w"
      "mod x 2"
      "div w 2"
      "mod w 2")
     (map parse-line)
     (run-program (list 11)))

(reduce instruction-eval (initial-state (list 1 2 3)) [["inp" "x"] ["inp" "w"] ["add" "w" "x"]])

(def parsed-program
  (->> (map parse-line (utils/read-input "day24.txt"))
       (map-indexed vector)
       (vec)))

(defn to-model-seq [num]
  (let [num-str (str num)
        padding (- 14 (.length num-str))]
    (->> (.concat ^String (string/join (repeat padding "0")) num-str)
         (seq)
         (map #(Integer/valueOf (str %))))))

(defn to-flow-graph [program]
  (reduce (fn [[nodes adjacency registers] [n instr]]
            (if (= (first instr) "inp")
              [(conj nodes [n instr])
               adjacency
               (assoc registers (second instr) [n instr])]
              (let [op1 (second instr)
                    op2 (-> instr (rest) (rest) (first))]
                [(conj nodes [n instr])
                 (if (or (nil? op2) (number? op2))
                   (assoc adjacency [n instr] [(get registers op1 0)])
                   (assoc adjacency [n instr] [(get registers op1 0) (get registers op2 0)]))
                 (assoc registers op1 [n instr])])))
          [[] {} {}]
          program))

(reduce conj [1] [])

(defn prune-graph [[nodes adjacency registers]]
  ;; we only care about nodes that lead back to this one.
  (let [final (last nodes)
        live-instructions
        (loop [queue [final]
               live-instructions #{}
               i 0]
          (if (> i 1000) live-instructions
              (if (empty? queue)
                live-instructions
                (let [node (first queue)]
                  (recur
                   (reduce conj (subvec queue 1)
                           (if (contains? live-instructions node)
                             []
                             (get adjacency node [])))
                   (conj live-instructions node)
                   (inc i))))))]
    [(filter #(contains? live-instructions %) nodes)
     adjacency
     registers]))

(to-flow-graph parsed-program)

(defn to-graphviz [[nodes adjacency actual-value-map]]
  (loop [nodes nodes
         graphviz-string "digraph G {\n\tNode0 [label=\"0\"];\n"
         node-num 1
         instr-nums {0 0}]
    (if (empty? nodes)
      (.concat graphviz-string "}\n")
      (recur
       (rest nodes)
       (-> graphviz-string
           (.concat (format "\tNode%d [label=\"%d %s (computed: %s)\"];\n" node-num node-num (.replaceAll (str (second (first nodes))) "\"" "")
                            (let [v (actual-value-map (first nodes))] (cond (number? v) v
                                                                            (and (vector? v) (= (first v) "inp")) (.replaceAll (str v) "\"" "")
                                                                            :else nil))))
           ;; point it at its predecessors.
           (.concat (string/join "\n" (map (fn [instr] (format "\tNode%d -> Node%d;" (get instr-nums instr 0)  node-num)) (adjacency (first nodes)))))
           (.concat "\n"))
       (inc node-num)
       (assoc instr-nums (first nodes) node-num)))))

;; we'll replace nodes with other nodes based on what we think is happening.
;; the adjacency list needs to stay the same (??)
;; we just want to create map the nodes to the "real" nodes.
;; then that map can be used for display, I guess, or we can transform the
;; adjacency graph.

(defn symbolic-eval [expr]
  (match expr
    ["inp" _] expr
    ["mul" ['number 0] _] ['number 0]
    ["mul" _ ['number 0]] ['number 0]
    ["mul" ['number 1] op] op
    ["mul" op ['number 1]] op
    ["mul" ['number n] ['number m]] ['number (* n m)]
    ["mul" ["mul" op1 op2] op3] ["mul" op1 (symbolic-eval ["mul" op2 op3])]
    ["add" ['number 0] op] op
    ["add" op ['number 0]] op
    ["add" ['number m] ['number n]] ['number (+ m n)]
    ;; should I also symbolic-eval the result here?
    ["add" ["add" op1 op2] op3] ["add" op1 (symbolic-eval ["add" op2 op3])]
    ["div" op ['number 1]] op
    ["div" ['number 0] _] ['number 0]
    ["div" ['number m] ['number n]] ['number (quot m n)]
    ["eql" ['number m] ['number n]] ['number (if (= m n) 1 0)]
    ["eql" ["inp" _] ['number n]] (if (not (<= 1 n 9)) ['number 0] expr)
    ;; not being tail recursive explodes the stack. this is kind of nice vs the
    ;; behavior calva has with my infinite loops :/
    ["eql" ['number _] ["inp" _]] (symbolic-eval ["eql" (nth expr 2) (second expr)])
    ["eql" ["inp" _] ["add" ["inp" _] ['number n]]] (if (>= n 9) ['number 0] expr)
    ["eql" ["add" ["inp" _] ['number n]] ["inp" _]] (if (>= n 9) ['number 0] expr)
    ["eql" o1 o2] (if (= o1 o2) ['number 1] expr)
    ["mod" ['number 0] _] ['number 0]
    ["mod" ['number n] ['number m]] ['number (mod n m)]
    ["mod" ["inp" o] ['number m]] (if (> m 9) ["inp" o] expr)
    ["mod" ["add" ["inp" o] ['number m]] ['number n]] (if (< (+ m 9) n) ["inp" o] expr)
    ["mod" ["add" ["mul" _ ['number m]] ["add" ["inp" _] ['number n]] :as op] ['number o]]
    (if (and (= m o) (< (+ n 9) o))
      op
      expr)
    :else expr))

(assert (= (symbolic-eval ["mul" ['number 4] ['number 4]]) ['number 16]))
(assert (= (symbolic-eval ["eql" ["inp" 1] ['number 26]]) ['number 0]))
(assert (= (symbolic-eval ["eql" ['number 26] ["inp" 1]]) ['number 0]))
(assert (= (symbolic-eval ["eql" ["inp" 1] ['number 1]])
           ["eql" ["inp" 1] ['number 1]]))


(symbolic-eval ["add" "x" ['number 14]])

(symbolic-eval ["mul" ['number 4] ['number 4]])

(defn symbolic-execution [[nodes' adjacency' _]]
  (loop
   [nodes nodes'
    register {"w" ['number 0] "x" ['number 0] "y" ['number 0] "z" ['number 0]}
    node-map {}
    register-map {}
    input-num 1
    i 0]
    (cond
      (empty? nodes) [nodes' adjacency' node-map register-map]
      ;; (> i 50) ['nodes adjacency' node-map]
      :else (let [[n instr] (first nodes)
                  [opcode op1 op2] instr
                  op1 (register op1)
                  get-binding (fn [x] (if (string? x) (register x) x))
                  ;; _ (println "op2" op2)
                  op2 (get-binding op2)
                  new-value (if (= opcode "inp")
                              instr
                              (symbolic-eval (if (nil? op2)
                                             (assoc instr 1 op1)
                                             (-> instr (assoc 1 op1) (assoc 2 op2)))))
                  ;; _ (println [n instr] ">>>" new-value "<<<")
                  ]
              (recur
               (rest nodes)
               (assoc register (second instr) new-value)
               (assoc node-map [n instr] new-value)
               (assoc register-map [n instr] register)
               (case opcode "inp" (inc input-num) input-num)
               (inc i))))))

(->> (-> parsed-program
         (to-flow-graph)
         (prune-graph)
         (symbolic-execution)
         (last))
     (filter (fn [[[n _] _]] (= n 43))))

(-> parsed-program
    (to-flow-graph)
    (prune-graph)
    (symbolic-execution)
    (to-graphviz)
    (println))

    ;; (to-graphviz)
    ;; (println))

;; (->> (range 11111111111111 Long/MAX_VALUE)
;;      (map #(vector % (to-model-seq %)))
;;      (remove #(some zero? (second %)))
;;      (map #(update % 1 (fn [input] (run-program input parsed-program)))))

;;      (filter #(= (get-in % [1 "z"]) 0)))

;;      (first)
;;      )



(def z-mul-positions
  (->> parsed-program
       (filter #(match [(second %)] [["mul" "z" _]] true :else false))
       (map first)))

(def eq-x-0-positions
  (->> parsed-program
       (filter #(match [(second %)] [["eql" "x" ['number 0]]] true :else false))
       (map first)
       (set)))

(def input-positions
  (->> parsed-program
       (filter #(= (second %) ["inp" "w"]))
       (map first)
       (set)))

(defn program-seq [input]
  (let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                          (vec))]
    (iterate
     (fn [[state program]]
       (if (empty? program)
         [state program]
         [(instruction-eval state (first program))
          (rest program)]))
     [(initial-state input) full-program])))

;; difference in 1 308915776  (26^6)
;; difference in 2 11881376 (26^5)
;; third has no effect, is immediately modded to 0
;; difference in 4 456976 (26^4)
;; difference in 5 17576 (26^3)
;; difference in 6 676 (26^2)
;; seven has no effect
;; [I don't know what these are doing, they aren't independent of each other like the base ones]

;; ;; OK, so 8 - 13 is key to the puzzle.
;; ;; need to understand how these contribute to z

;; difference in 8 -2780246456 WHEN 2 or 8
;; difference in 9 -2780246612 WHEN 2 or 8
;;      2 ^2 * 11 * 13 * 1093 * 4447
;; tenth has no effect, immediately compared to something it can't be.
;; 11th has a similar odd distance - also % 0 mod 26
;;      2 ^2 * 3 * 11 * 13 * 54563
;;      when -3 ???
;; these high primes can't be part of the ALU since we don't have constants that size.
;; 12th has a difference of -93630290
;;      2 * 5 * 13 * 19 * 37907
;; 13th also difference of -2 * 5 * 13 * 19 * 37907
;; 14th does not effect z, but needs to make it so z equals y.

(count '(1 1 1 1 1 1 1 1 3 7 6 1 6 9))

;; (run-full-program (repeat 14 0))

;; OK, fiddling with this gives me
;; 27375125691319 is an accepted number.
;; of course this is not the solution.
;; still don't really understand what's going on :-) -> yet
(->>  '([1 9] [2 9] [3 5] [4 9] [5 1] [6 1] [7 2] [8 5] [9 6] [10 9] [11 1] [12 3] [13 1] [14 1])
      (map second)
      (program-seq)
      (take (inc (count parsed-program)))
      (map first)
      (map #(dissoc % :input))
      (map-indexed vector)
      (filter #(or (contains? eq-x-0-positions (dec (first %)))
                   (<= 37 (first %) 67)
                   (= (first %) (count parsed-program)))))
      ;; (map second))
      ;; (map #(vector (nth parsed-program (dec (first %))) (second %))))

;; this is where the "z" value ends up after the first two inputs.
;; third input does not contribute to "z", except through "y".
(defn first-two [i0 i1]
  (+ (* (+ 7 i0) 26)
     (+ 4 i1)))

;; x needs to be 0 (line 65)
;; fourth input is how we avoid a multiplication by 26 (line 67)
;; must be equal to first 2 mod 26 - 4
(- (mod (first-two 9 7) 26) 4)

(defn num-four [i0 i1 i2]
  (- (mod (+ (+ (* 26 (first-two i0 i1)) 8) i2) 26) 4))

(num-four 9 9 5)
;; PATH TO BEING DONE: write a function that searches suffixes.
;; figure out how the first N numbers relate enough that I can brute force the
;; rest.

;; all suffixes
(apply combo/cartesian-product (repeat 14 (range 1 10)))

(defn brute-force-suffixes [prefix]
  (let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                          (vec))]
    (->>
     (for [suffix (apply combo/cartesian-product (repeat (- 14 (count prefix)) (range 1 10)))]
      (-> (run-program (concat prefix suffix) full-program)
          (get "z")
          (vector suffix)))
     (reduce (fn [sofar x] (if (< (first x) (first sofar)) x sofar)))
     ((fn [[n suffix]] (vector n (concat prefix suffix)))))))

(brute-force-suffixes '(2 7 3 7 5 1 2 5 9))

(let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                        (vec))
      initial-mins []]
  (loop
   [positions (drop (count initial-mins) z-mul-positions)
    mins-sofar initial-mins]
    (if (empty? positions) mins-sofar
        (recur
         (rest positions)
         (apply conj mins-sofar
                (->> (for [n (repeatedly 100 #(inc (int (rand 9))))]
                       [[n] (get "z" (run-program (conj mins-sofar n) (take (inc (first positions)) full-program)))])
                     (sort-by #(second (second %)))
                     (first)
                     (first)))))))

;; [7 5 1 5 7 5 2 5 2 3 8 7 5 9]

(defn run-full-program [input]
  (let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                          (vec))]
    (run-program input full-program)))

(run-full-program '(2 7 3 7 5 1 2 5 6 9 1 3 1 9))
(run-full-program '(4 9 5 9 7 3 4 7 8 9 3 5 3 1))

;; 27375125691319 is an accepted number.

((->> (program-seq (list 2 7 3 7 5 1 2 5 6 9 1 3 2 6))
     (partition-by #(empty? (second %)))) false)


(run-full-program [2 4 1 2 9 9 9 4 6 1 8
                   9 9 4])
(run-full-program [1 3 5 7 9 2 4 6 8 9 9 9 9 9])
(let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                        (vec))
      initial-mins [9 3 5 4 3 2 1 0 4]]
  (loop
   [positions (drop (count initial-mins) z-mul-positions)
    mins-sofar initial-mins]
    (println positions mins-sofar)
    (if (empty? positions) mins-sofar
        (recur
         (rest positions)
         (apply conj mins-sofar
                (->> (for [n (inc (int (rand 9)))
                           m (range 1 10)]
                       [[n m] (run-program (conj mins-sofar n) (take (inc (first positions)) full-program))])
                     (sort-by #(second (second %)))
                     (first)
                     (first)))))))

(let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                        (vec))]
  (run-program (list 1 1 1 5 1 1 1 4 6 1 1 1 1 5) full-program))

;;   (->>
;;    (for [n (range 1 10)
;;          m (range 1 10)
;;          o (range 1 10)
;;          p (range 1 10)
;;          q (range 1 10)]
;;      (-> (run-program (list n m o p q) (take 67 full-program))
;;          (get "z")
;;          (vector [n m o p q])))
;;   ;;  (map (fn [[n & args]] (concat [(mod n 26)] args)))
;;    (sort-by first)
;;    (take 5)))

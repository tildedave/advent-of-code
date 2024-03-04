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
       (mapv utils/try-parse-int)))

;; good enough
(defn instruction-eval [state prog]
  (let [num (fn [var-or-num] (if (number? var-or-num) var-or-num (state var-or-num)))]
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
                               (update v #(if (= % (num var-or-num)) 1 0)))
      )))

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
     (run-program (list  2 7)))

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

(reduce instruction-eval (initial-state (list 1 2 3)) [["inp" "x"] ["inp" "w"]["add" "w" "x"]])

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

(seq "123")

(to-model-seq 12)

(-> (list 1 2) (rest) (rest) (first))

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

(defn symbolic-execution [[nodes' adjacency' _]]
  (loop
   [nodes nodes'
    register {"w" 0 "x" 0 "y" 0 "z" 0}
    node-map {}
    input-num 1
    i 0]
    (cond
      (empty? nodes) [nodes' adjacency' node-map]
      :else (let [[n instr] (first nodes)
                  [opcode op1 op2] instr
                  op1 (register op1)
                  get-binding (fn [x] (cond
                                        (nil? x) nil
                                        (number? x) x
                                        :else (register x)))
                  op2 (get-binding op2)
                  ;; _ (if (= opcode "eql") (println instr register))
                  new-value (condp = opcode
                              "inp" ["inp" input-num]
                              "mul" (match [op1 op2 (number? op1) (number? op2)]
                                      [0 _ _ _] 0
                                      [_ 0 _ _] 0
                                      [1 _ _ _] op2
                                      [_ 1 _ _] op1
                                      [_ _ true true] (+ op1 op2)
                                      :else ["mul" op1 op2])
                              "add" (match [op1 op2 (number? op1) (number? op2)]
                                      [0 _ _ _] op2
                                      [_ 0 _ _] op1
                                      [_ _ true true] (+ op1 op2)
                                      ;; obviously need to look up n here.
                                      [["add" x y] m _ true]
                                      (let [n (get-binding y)]
                                        (if (number? n)
                                          ["add" x (+ n m)]
                                          ["add" op1 op2]))
                                      [["add" x y] ["add" q r] _ _]

                                      :else ["add" op1 op2])

                              "div" (match [op1 op2 (number? op1) (number? op2)]
                                      [0 _ _ _] 0
                                      [_ 1 _ _] op1
                                      [_ _ true true] (quot op1 op2)
                                      :else ["div" op1 op2])
                              "eql" (cond
                                      (= op1 op2) 1
                                      (match [op1 op2 (number? op1) (number? op2)]
                                        [["inp" _] _ _ true] (or (= op2 0) (> op2 9))
                                        [_ ["inp" _] true _] (or (= op1 0) (> op1 9))
                                        ;; if you're adding over 10 and comparing, it's a 0.
                                        [["add" ["inp" _] x] ["inp" _] _ _]
                                        (let [n (get-binding x)]
                                          (and (number? n) (> n 9)))
                                        [["inp" _] ["add" ["inp" _] x] _ _]
                                        (let [n (get-binding x)]
                                          (and (number? n) (> n 9)))
                                        :else false) 0
                                      :else ["=" op1 op2])
                              "mod" (cond
                                      (= op1 0) 0
                                      (and (number? op1) (number? op2)) (mod op1 op2)
                                      (match [op1 (number? op2)]
                                        [["inp" _] true] true
                                        [["add" ["inp" _] x] true]
                                        (let [n (get-binding x)]
                                          (and (number? n) (< (+ n 9) op2)))
                                        :else false) op1
                                      :else ["mod" op1 op2])
                              :else "!!!!INVALID!!!!"
                              )]
              (recur
               (rest nodes)
               (assoc register (second instr) new-value)
               (assoc node-map [n instr] new-value)
               (case opcode "inp" (inc input-num) input-num)
               (inc i))))))

(comment
  ;; = number? - Example 1 =

  user=> (number? 1)
  true
  user=> (number? 1.0)
  true
  user=> (number? :a)
  false
  user=> (number? nil)
  false
  user=> (number? "23")
  false


  ;; See also:
  clojure.core/num
  clojure.core/integer?
  :rcf)


(->> (-> parsed-program
         (to-flow-graph)
         (prune-graph)
         (symbolic-execution)
         (last))
     (filter (fn [[[n _] _]] (= n 39))))

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

(conj '(2 3) 1)

(repeatedly 10 #(inc (int (rand 9))))

(let [full-program (->> (map parse-line (utils/read-input "day24.txt"))
                        (vec))
      initial-mins []]
  (loop
   [positions (drop (count initial-mins) z-mul-positions)
    mins-sofar initial-mins]
    (println positions mins-sofar)
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

(run-full-program [2 4 1 2 9 9 9 4 6 1 8 9 9 4])

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

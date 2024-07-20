(ns advent2023.day19
  (:require [utils :as utils]
            [graph :as graph]))

(defn parse-comparison [^String s]
  (let [[attr comp num] (rest (re-matches #"^(x|m|a|s)(<|>)([0-9]+)$" s))]
    [(case comp
       ">" :gt
       "<" :lt)
     attr
     (utils/parse-int num)]))

(defn to-next [s]
  (case s
    "R" :reject
    "A" :accept
    s))

(defn parse-workflow-rule [line]
  (let [[workflow ^String transition-list] (rest (re-matches #"^([a-z]+)\{(.*)\}" line))]
    {workflow (map
               (fn [^String s]
                 (let [r (.split s ":")]
                   (if
                    (= (count r) 1) (to-next s)
                    [(parse-comparison (first r))
                     (to-next (second r))])))
               (.split transition-list ","))}))

(defn parse-part [line]
  (->> line
       (re-matches #"^\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}$")
       (rest)
       (map utils/parse-int)
       ((fn [[x m a s]] {:x x :m m :a a :s s}))))

(parse-part "{x=787,m=2655,a=1222,s=2876}")

(parse-workflow-rule "px{a<2006:qkq,m>2090:A,rfg}")

(defn parse-input [lines]
  (let [[workflows parts] (utils/split-by "" lines)]
    {:workflows (reduce merge {} (map parse-workflow-rule workflows))
     :parts (map parse-part parts)}))

(def example-input
  '("px{a<2006:qkq,m>2090:A,rfg}"
    "pv{a>1716:R,A}"
    "lnx{m>1548:A,A}"
    "rfg{s<537:gd,x>2440:R,A}"
    "qs{s>3448:A,lnx}"
    "qkq{x<1416:A,crn}"
    "crn{x>2662:A,R}"
    "in{s<1351:px,qqz}"
    "qqz{s>2770:qs,m<1801:hdj,R}"
    "gd{a>3333:R,R}"
    "hdj{m>838:A,pv}"
    ""
    "{x=787,m=2655,a=1222,s=2876}"
    "{x=1679,m=44,a=2067,s=496}"
    "{x=2036,m=264,a=79,s=2244}"
    "{x=2461,m=1339,a=466,s=291}"
    "{x=2127,m=1623,a=2188,s=1013}"))

(parse-input example-input)

(defn eval-workflow [[comp attr num] part]
  ((case comp
     :lt <
     :gt >)
   (part (keyword attr))
   num))

(defn run-workflow [workflows part]
  (loop [workflow "in"]
    (let [result (->> (workflows workflow)
                      (filter (fn [w]
                                (or (string? w)
                                    (keyword? w)
                                    (eval-workflow (first w) part))))
                      (first))]
      (cond
        (string? result) (recur result)
        (keyword? result) result
        (keyword? (second result)) (second result)
        :else (recur (second result))))))

(defn part1 [lines]
  (let [{:keys [workflows parts]} (parse-input lines)]
    (->> parts
         (filter #(= :accept (run-workflow workflows %)))
         (map vals)
         (map #(reduce + %))
         (reduce +))))

;; correct
(part1 (utils/read-input "2023/day19.txt"))

;; OK so for part 2 I used a queue approach in golang
;; I suppose I will do the same thing
;; really this is BFS

(defn invert-workflow [[comp attr num]]
  [(case comp
     :lt :gt
     :gt :lt)
   attr
   (case comp
     :gt (inc num)
     :lt (dec num))])

(defn path-neighbors [workflows current-path]
  (let [last-node (last current-path)]
    (if (= last-node :accept)
      '()
      (first
       (reduce
        (fn [[neighbors current-path] workflow-item]
          (cond
            (or (string? workflow-item) (= :accept workflow-item))
            [(conj neighbors (conj current-path workflow-item))
             current-path]
            (= :reject workflow-item)
            [neighbors current-path]
            :else
            [(conj neighbors
                   (-> current-path
                       (conj (first workflow-item))
                       (conj (second workflow-item))))
             (conj current-path (invert-workflow (first workflow-item)))]))
        [[] current-path]
        (workflows last-node))))))

(defn num-paths [path]
  (reduce
   (fn [criteria [comp attr num]]
     (update
      criteria
      attr
      (fn [[old-min old-max]]
        (case comp
          :gt [(max old-min (inc num)) old-max]
          :lt [old-min (min old-max (dec num))]))))
   {:x [1 4000] :m [1 4000] :a [1 4000] :s [1 4000]}
   (map #(assoc % 1 (keyword (% 1))) (filter vector? path))))

;; part 2
(let [{:keys [workflows]} (parse-input (utils/read-input "2023/day19.txt"))]
  (->>
   (graph/breadth-first-search
    ["in"]
    (partial path-neighbors workflows))
   (second)
   (map first)
   (remove
    (fn [v] (not= (last v) :accept)))
   (map num-paths)
   (map #(update-vals % (fn [[min max]] (inc (- max min)))))
   (map #(reduce * (map second %)))
   (reduce +)))

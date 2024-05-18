(ns advent2017.day25
  (:require [utils :as utils]))

(defn parse-instr [s]
  (if-let [[_ write-val] (re-find #"\- Write the value (\d+).$" s)]
    [:write-value (utils/parse-int write-val)]
    (if-let [[_ move-dir] (re-find #"\- Move one slot to the (right|left).$" s)]
      [:move (case move-dir "left" -1 "right" 1)]
      (if-let [[_ next-state] (re-find #"\- Continue with state (\w).$" s)]
        [:next-state next-state]
        (throw (Exception. (format "Could not parse %s" s)))))))

(defn parse-state [state-lines]
  (let [header (->> (first state-lines)
                    (re-matches #"^In state (\w):$")
                    (rest)
                    (first))
        [if-curr-value-0 if-curr-value-1]
        (->> (rest state-lines)
             (partition 4))]

    {header {0 (map parse-instr (rest if-curr-value-0)) 1
    (map parse-instr (rest if-curr-value-1))}}))

(defn parse-blueprints [filename]
  (let [[[begin-in-state checksum-str] & state-list]
        (->> (utils/read-input filename)
             (utils/split-by ""))
        starting-state (->> begin-in-state
                            (re-matches #"^Begin in state (\w).$")
                            (second))
        check-num (->> checksum-str
                       (re-matches #"^Perform a diagnostic checksum after (\d+) steps.$")
                       (second)
                       (utils/parse-int))]
    {:checksum-at check-num :starting-state starting-state
     :states (reduce merge (map parse-state state-list))}))

(parse-blueprints "2017/day25-example.txt")

(def initial-state
  {:ones #{}
   :position 0
   :state "A"})

(defn simulate-instr [curr-state [instr x]]
    (case instr
      :write-value
      (let [{:keys [position]} curr-state]
        (case x
          0 (update curr-state :ones #(disj % position))
          1 (update curr-state :ones #(conj % position))))
      :move (update curr-state :position (partial + x))
      :next-state (assoc curr-state :state x)))

(defn simulate [machine-definition]
  (fn [curr-state]
    (let [{:keys [ones position state]} curr-state
          state-def (get-in machine-definition [:states state])]
      (reduce
       simulate-instr
       curr-state
       (state-def
        (case (contains? ones position)
          true 1
          false 0))))))

(defn answer [machine-definition]
  (-> (iterate (simulate machine-definition) initial-state)
      (nth (get machine-definition :checksum-at))
      :ones
      (count)))

(parse-blueprints "2017/day25-example.txt")
(answer (parse-blueprints "2017/day25-example.txt"))
(answer (parse-blueprints "2017/day25.txt"))

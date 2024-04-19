(ns advent2015.day23
  (:require [utils :as utils]
            [clojure.core.match :refer [match]]))

(def ^:dynamic part2? false)

(defn parse-instruction [line]
  (let [[instr r] (.split line " " 2)]
    (case instr
      ("hlf" "tpl" "inc") [instr r]
      ("jmp") [instr (utils/parse-int r)]
      ("jie" "jio") (let [[r offset] (.split r ", " 2)]
                   [instr r (utils/parse-int offset)]))))

(defn process-instruction [state]
  (let [{:keys [idx program]} state
        [instr r offset] (get program idx)]
    (case instr
      "hlf" (-> state
                (update r #(quot % 2))
                (update :idx inc))
      "tpl" (-> state
                (update r #(* % 3))
                (update :idx inc))
      "inc" (-> state
                (update r inc)
                (update :idx inc))
      "jmp" (-> state (update :idx (partial + r)))
      "jie"
      (case (mod (get state r) 2)
        0 (update state :idx (partial + offset))
        (update state :idx inc))
      "jio"
      (case (get state r)
        1 (update state :idx (partial + offset))
        (update state :idx inc))
        )))

(defn initial-state [filename]
  (->> filename
       (utils/read-input)
       (mapv parse-instruction)
       (assoc {"a" (if part2? 1 0) "b" 0 :idx 0} :program)))

(defn program-seq [filename]
  (iterate process-instruction (initial-state filename)))

(defn terminal-state [filename]
  (reduce
   (fn [acc state]
     (let [{:keys [program idx]} state]
       (if  (< -1 idx (count program))
         nil
         (reduced state))))
   (program-seq filename)))

(terminal-state "2015/day23-example.txt")
(dissoc (terminal-state "2015/day23.txt") :program)

(binding [part2? true]
  (dissoc (terminal-state "2015/day23.txt") :program))

(->> (program-seq "2015/day23.txt")
     (map #(assoc % :about-to-execute (get-in % [:program (:idx %)])))
     (map #(dissoc % :program))
     (take 24))
(process-instruction (initial-state "2015/day23-example.txt"))

(take 4 (program-seq "2015/day23-example.txt"))

(->> (utils/read-input "2015/day23-example.txt")
     (map parse-instruction))

(parse-instruction "jio a, +2")

(parse-instruction "hlf r +123")

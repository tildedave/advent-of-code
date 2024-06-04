(ns advent2018.day16
  (:require [clojure.core.async :as a]
            [utils :as utils]
            [clojure.set :as set]))

(def opcodes
  '(:addr :addi :mulr :muli :banr :bani :borr :bori
          :setr :seti :gtir :gtri :gtrr :eqir :eqri :eqrr))

(defn process-instruction [state [interpreted-opcode a b c]]
  (assoc
   state c
   (case interpreted-opcode
     :addr (+ (state a) (state b))
     :addi (+ (state a) b)
     :mulr (* (state a) (state b))
     :muli (* (state a) b)
     :banr (bit-and (state a) (state b))
     :bani (bit-and (state a) b)
     :borr (bit-or (state a) (state b))
     :bori (bit-or (state a) b)
     :setr (state a)
     :seti a
     :gtir (if (> a (state b)) 1 0)
     :gtri (if (> (state a) b) 1 0)
     :gtrr (if (> (state a) (state b)) 1 0)
     :eqir (if (= a (state b)) 1 0)
     :eqri (if (= (state a) b) 1 0)
     :eqrr (if (= (state a) (state b)) 1 0))))

;; so let's assume this is all correct

(defn candidates [state-before full-instruction state-after]
  (let [[opcode] full-instruction]
    {opcode (->> opcodes
                 (filter #(= (process-instruction state-before (assoc full-instruction 0 %))
                             state-after))
                 (set))}))

(candidates [3 2 1 1] [9 2 1 2] [3 2 2 1])

(def magic-separator 3123)

(defn parse-number-list [num-list-str]
  (mapv utils/parse-int
        (-> num-list-str
            (second)
            (.replaceAll "\\[" "")
            (.replaceAll "\\]" "")
            (.split ", "))))

(defn parse-sample [[^String before full-instr-str ^String after]]
    [(parse-number-list (.split before "Before: "))
     (mapv utils/parse-int (.split full-instr-str " "))
     (parse-number-list (.split after "After:  "))])

(defn answer-part1 []
  (->> (take magic-separator (utils/read-input "2018/day16.txt"))
       (partition 4)
       (map (partial take 3))
       (map parse-sample)
       (map #(apply candidates %))
       (filter #(>= (count (first (vals %))) 3))
       (count)))

    ;;    (reduce (partial merge-with set/intersection))))

(answer-part1)

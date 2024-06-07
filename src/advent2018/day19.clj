(ns advent2018.day19
  (:require [utils :as utils]
            [advent2018.day16 :as day16]))

(defn parse-instruction [^String line]
  (let [[opcode a b c] (.split line " ")]
    [(keyword opcode)
     (utils/parse-int a)
     (utils/parse-int b)
     (utils/parse-int c)]))

(parse-instruction "seti 5 0 1")

(defn process-instruction [state]
  (let [{:keys [registers program ip-register ip]} state]
    (if (= ip -1)
      state
      (let [next-registers (day16/process-instruction
                            (assoc registers ip-register ip)
                            (get program ip))
            next-ip (inc (next-registers ip-register))]
        (-> state
            (assoc :registers next-registers)
            (assoc :ip (if (contains? program next-ip) next-ip -1)))))))


(defn parse-file [filename]
  (->> (utils/read-input filename)
       ((fn [[ip-line & rest]]
          {:program (mapv parse-instruction rest)
           :ip-register (->> ip-line
                             (re-matches #"^#ip (\d+)$")
                             (second)
                             (utils/parse-int))
           :ip 0
           :registers {0 0 1 0 2 0 3 0 4 0 5 0}}))))

(parse-file "2018/day19.txt")

(defn answer [filename register extra]
  (->> (parse-file filename)
       ((fn [state] (-> state
                        (update :registers #(merge % extra)))))
       (iterate process-instruction)
       (reduce
        (fn [acc {:keys [ip registers]}]
          (if (= ip -1)
            (reduced (registers register))
            acc))
        nil)))

(answer "2018/day19.txt" 0 {0 1})

(->> (parse-file "2018/day19.txt")
     ((fn [state] (-> state
                      (update :registers #(merge % {0 1})))))
     (iterate process-instruction)
     (map :registers))

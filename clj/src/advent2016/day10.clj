(ns advent2016.day10
  (:require [utils :as utils]))

(def value-re #"^value (\d+) goes to bot (\d+)$")
(def branch-re #"^bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)$")

(defn parse-line [s]
  (if-let [[val bot] (->> s
                            (re-matches value-re)
                            (#(if (nil? %) %
                                  (map utils/parse-int (rest %)))))]
    {:input [[bot val]]}
    (if-let [[bot low-type low high-type high] (->> s
                                       (re-matches branch-re)
                                       (#(if (nil? %) %
                                        (map utils/try-parse-int (rest %)))))]
      {bot {:low [low-type low] :high [high-type high]}}
      (throw (Exception. (format "Could not parse %s" s))))))

(parse-line "bot 2 gives low to bot 1 and high to bot 0")

(defn parse-system [lines]
  (->> lines
       (map parse-line)
       (reduce (partial merge-with concat))))

(defn initial-state [system]
  (assoc
   (->> system
       (map (fn [[k v]] (if (= k :input) [k v] [k []])))
       (into {}))
   :output {}))

(->> (utils/read-input "2016/day10.txt") (parse-system))

(defn act [system state]
;;   (println "I state!" state)
  (if-let [[bot val] (first (:input state))]
    [(-> state
        (update :input rest)
        (update bot #(conj % val))
        )
     :fed-input]
    ;; otherwise find a bot with 2 chips and do the logic.
    (if-let [bot-with-two-chips (->> (dissoc state :input :output)
                                     (filter (fn [[_ v]] (= (count v) 2)))
                                     (first)
                                     (first))]
      (let [ ;_ (println bot-with-two-chips)
            [chip1 chip2] (state bot-with-two-chips)
            {:keys [low high]} (system bot-with-two-chips)
            [low-type low] low
            [high-type high] high
            low-chip (min chip1 chip2)
            high-chip (max chip1 chip2)]
        [(cond-> (assoc state bot-with-two-chips [])
          (= low-type "bot") (update low (fnil #(conj % low-chip) []))
          (= low-type "output") (update-in [:output low] (fnil #(conj % low-chip) []))
          (= high-type "bot") (update high (fnil #(conj % high-chip) []))
          (= high-type "output") (update-in [:output high] (fnil #(conj % high-chip) [])))
         :bot-comparison bot-with-two-chips low-chip high-chip
         ])
      [state :terminal])))

(defn state-seq [lines]
  (let [system (parse-system lines)]
    (iterate (fn [[state]] (act system state))
             [(initial-state system)])))

(state-seq ["value 5 goes to bot 2"
            "bot 2 gives low to bot 1 and high to bot 0"
            "value 3 goes to bot 1"
            "bot 1 gives low to output 1 and high to bot 0"
            "bot 0 gives low to output 2 and high to output 0"
            "value 2 goes to bot 2"])

(defn answer-part1 []
  (->> (state-seq (utils/read-input "2016/day10.txt"))
       (filter (fn [[_ kw]] (= kw :bot-comparison)))
       (map rest)
       (filter (fn [[_ _ chip1 chip2]] (and (= chip1 17) (= chip2 61))))
       (first)
       (rest)
       (first)))

(answer-part1)
(defn answer-part2 []
  (->> (state-seq (utils/read-input "2016/day10.txt"))
       (reduce (fn [_ [state status]] (if (= status :terminal) (reduced state) nil)))
       (:output)
       (#(reduce * (map first [(% 0) (% 1) (% 2)]))
       )))

(answer-part2)

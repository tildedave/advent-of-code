(ns advent2019.day23
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]))

(defn boot-nic []
  (let [answer-part1 (a/chan 1)
        answer-part2 (a/chan 1)
        program (-> (intcode/parse-file "2019/day23.txt")
                    (assoc :default-input -1)
                    ;; (assoc :channel-debug? true)
                    )
        system (->> (range 50)
                    (map
                     (fn [n]
                       (let [input (a/chan 1024)
                             output (a/chan)]
                         {n {:input input :output output}})))
                    (reduce merge {}))]
    (let [output-to-n (->> system
                           (map (fn [[n {:keys [output]}]]
                                  {output n}))
                           (reduce merge {}))
          input-0 (:input (system 0))]
      (a/go-loop
       [last-nat-packet nil
        last-sent-nat-packet nil]
        (let [timeout-chan (a/timeout 300)
              [dest ch] (a/alts! (conj (vec (keys output-to-n)) timeout-chan))]
          (if (= ch timeout-chan)
            (do
              (println "[NAT] sending" last-nat-packet "to address 0")
              (>! input-0 (first last-nat-packet))
              (>! input-0 (second last-nat-packet))
              (if (= last-nat-packet last-sent-nat-packet)
                (>! answer-part2 (second last-sent-nat-packet))
                nil)
              (recur last-nat-packet last-nat-packet))
            ;; NAT packet to address 0
            (let [x (<! ch)
                  y (<! ch)]
              (if (= dest 255)
                (do
                  (if (nil? last-nat-packet)
                    (>! answer-part1 [x y])
                    nil)
                  (recur [x y] last-sent-nat-packet))
                (let [input (:input (system dest))]
                  (.println *err* (format "[%d] sending (%d, %d) to %d" (output-to-n ch) x y dest))
                  (>! input x)
                  (>! input y)
                  (recur last-nat-packet last-sent-nat-packet)))))))
      (let [input-to-n (->> system
                            (map (fn [[n {:keys [input]}]]
                                   {input n}))
                            (reduce merge {}))]
        (doseq [{:keys [input output]} (vals system)]
          (>!! input (input-to-n input))
          (intcode/run-program (assoc program :program-id (input-to-n input)) input output)))
      [answer-part1 answer-part2])))

(let [[part1 part2] (boot-nic)]
  (println "part1:"  (<!! part1))
  (println "part2:"  (<!! part2)))

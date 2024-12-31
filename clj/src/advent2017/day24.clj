(ns advent2017.day24
  (:require [utils :as utils]))

(defn parse-component [^String s]
  {:ports (->> (.split s "/") (map utils/parse-int))
   :id (random-uuid)})

(parse-component "3/5")

(defn parse-components [^String filename]
  (reduce
   (fn [acc component]
     (let [[l r] (:ports component)]
       (-> acc
           (update l (fnil #(assoc % (:id component) component) {}))
           (update r (fnil #(assoc % (:id component) component) {})))))
   {}
   (->> (utils/read-input filename) (map parse-component))))

(parse-components "2017/day24-example.txt")

(defn all-bridges
  ([components] (all-bridges components #{} 0 '()))
  ([components seen-ids port-to-match bridge-so-far]
   ;; for each valid option (components port-to-match)
   (let [valid-keys (->> (components port-to-match)
                         (keys)
                         (remove #(contains? seen-ids %)))]
       (if (empty? valid-keys)
         (list bridge-so-far)
         (apply concat
                (for [k valid-keys]
           (let [[l r] (get-in components [port-to-match k :ports])
                 next-port (if (= port-to-match l) r l)]
             (all-bridges
              components
              (conj seen-ids k)
              next-port
              (cons [l r] bridge-so-far)))))))))

(defn total-strength [bridge]
  (->> bridge
       (map #(apply + %))
       (reduce +)))

(defn answer-part1 [filename]
  (->> (all-bridges (parse-components filename))
       (map total-strength)
       (sort >)
       (first)))

(answer-part1 "2017/day24-example.txt")
(answer-part1 "2017/day24.txt")

(all-bridges (parse-components "2017/day24.txt"))

(defn answer-part2 [filename]
  (->> (all-bridges (parse-components filename))
       (sort-by count >)
       (partition-by #(count %))
       (first)
       (map total-strength)
       (sort >)
       (first)))

(answer-part2 "2017/day24-example.txt")
(answer-part2 "2017/day24.txt")

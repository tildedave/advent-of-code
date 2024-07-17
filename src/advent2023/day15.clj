(ns advent2023.day15
  (:require [utils :as utils]))

(int \H)
(defn run-HASH [s]
  (reduce
   (fn [curr ch]
     (mod (* (+ curr (int ch)) 17) 256))
   0
   (seq s)))

(run-HASH "HASH")

;; part 1
(->> (.split #"," (utils/read-input-line "2023/day15.txt"))
     (map run-HASH)
     (reduce +))

;; part 2 is easy as well.

(defn parse-label [^String s]
  (mapv utils/try-parse-int (.split #"-|=" s)))

(run-HASH "cm")

(def example "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn label-matches? [label [label2 _]]
  (= label label2))

(label-matches? "qp" ["qp" 3])

(defn update-box [curr-label val curr]
  (loop [n 0]
    (if (= n (count curr))
      (conj curr [curr-label val])
      (let [[el ev] (get curr n)]
        (if (= el curr-label)
          (assoc curr n [el val])
          (recur (inc n)))))))

;; part 2
(->> (.split #"," (utils/read-input-line "2023/day15.txt"))
     (map parse-label)
     (reduce
      (fn [boxes [curr-label val]]
        (if val
          (update boxes (run-HASH curr-label)
                  (fnil
                   (partial update-box curr-label val)
                   []))
          (update boxes (run-HASH curr-label) (fnil #(filterv (fn [r] (not (label-matches? curr-label r))) %) []))))
      {})
     (map
      (fn [[box-num contents]]
        (->> contents
             (map-indexed
              (fn [n [_ v]]
                (* (inc box-num) (inc n) v)))
             (reduce +))))
     (reduce +))


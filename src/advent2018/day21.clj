(ns advent2018.day21
  (:require [advent2018.day19 :as day19]))


;; (->> (assoc-in
;;       (day19/parse-file "2018/day21.txt")
;;       [:registers 0]
;;       0) ;; 10780777)
;;      (iterate day19/process-instruction)
;;      (map #(dissoc % :program))
;;      (map-indexed vector)
;;      (drop-while #(not= (:ip (second  %)) 28))
;;      )

(defn process-instruction [state]
  (let [{:keys [ip]} state]
    (if (= ip 17)
      (-> state
          (update-in [:registers 1] #(quot % 256))
          (assoc :ip 27))
      (day19/process-instruction state)
      )))

;; (->> (assoc-in
;;       (day19/parse-file "2018/day21.txt")
;;       [:registers 0]
;;       10780777)
;;      (iterate process-instruction)
;;      (drop-while #(not= (:ip %) -1)))
1
;; part 1 answer
(->> (day19/parse-file "2018/day21.txt")
     (iterate process-instruction)
     (map #(dissoc % :program))
     (filter #(= (:ip %) 16))
     (map #(vector (get-in % [:registers 1]) (get-in % [:registers 2])))
     (first)
     (first)
     (println))

(->> (day19/parse-file "2018/day21.txt")
     (iterate process-instruction)
     (map #(dissoc % :program))
     (filter #(= (:ip %) 16))
     (map #(vector (get-in % [:registers 1]) (get-in % [:registers 2])))
     (reduce
      (fn [[seen order] curr]
        (println seen)
        (if (contains? seen curr)
          (reduced (peek order))
          [(conj seen curr) (conj order curr)]))
        [#{} []]))


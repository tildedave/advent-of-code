(ns advent2019.day23
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]))

(defn boot-nic []
  (let [answer (a/chan)
        program (-> (intcode/parse-file "2019/day23.txt")
                    (assoc :default-input -1))
        system (->> (range 50)
                    (map
                     (fn [n]
                       (let [input (a/chan 1024)
                             output (a/chan 1)]
                         {n {:input input :output output}})))
                    (reduce merge {}))]
    (let [output-to-n (->> system
                           (map (fn [[n {:keys [output]}]]
                                  {output n}))
                           (reduce merge {}))]
      (a/go-loop
       []
        (let [_ (println "waiting")
              [dest ch] (a/alts! (keys output-to-n))
              x (<! ch)
              y (<! ch)]
          (if (= dest 255)
            (do
              (println "sending answer")
              (>! answer [x y]))
            (let [input (:input (system dest))]
              (.println *err* (format "[%d] sending (%d, %d) to %d" (output-to-n ch) x y dest))
              (>! input x)
              (>! input y)
              (recur))))))
    (let [input-to-n (->> system
                          (map (fn [[n {:keys [input]}]]
                                 {input n}))
                          (reduce merge {}))]
      (doseq [{:keys [input output]} (vals system)]
        (>!! input (input-to-n input))
        (intcode/run-program (assoc program :program-id (input-to-n input)) input output)))
    ;; we'll fan out for everything.
    (println "system booted")
    (<!! answer)))

(println (boot-nic))

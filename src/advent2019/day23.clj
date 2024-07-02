(ns advent2019.day23
  (:require [advent2019.intcode :as intcode]
            [clojure.core.async :as a :refer [<!! >! <! >!!]]))

(defn boot-system [program fan-input]
  (->> (range 50)
       (map
        (fn [n]
          (let [input (a/chan)
                output (a/chan)
                _ (intcode/run-program program input output)
                _ (println "booting" n)]
            (a/go-loop []
              (if-let [dest (<! output)]
                (let [x (<! output)
                      y (<! output)
                      _ (println (format "[%d] received (%d, %d) from %d" n x y dest))]
                  (>! fan-input {:dest dest :x x :y y :from n})
                  (recur))
                nil))
            (>!! input n)
            (println "booted!" n)
            {n {:input input :output output}})))
       (reduce merge {})))

(defn boot-nic []
  (let [program (-> (intcode/parse-file "2019/day23.txt")
                    (assoc :default-input -1)
                    (assoc :non-blocking-output? true))
        fan-input (a/chan)
        system (boot-system program fan-input)]
    (<!!
     (a/go-loop []
       (println "waiting on fan-input")
       (if-let [{:keys [dest x y]} (<! fan-input)]
         (do
           (println "[nic] sending" x y "to" dest)
           (>! (:input (system dest)) x)
           (>! (:input (system dest)) y)
           (recur))
         nil)))))

(boot-nic)

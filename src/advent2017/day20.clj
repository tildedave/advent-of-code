(ns advent2017.day20
  (:require [utils :as utils]))

(defn parse-particle [line]
  (if-let [m (->> line
                  (re-matches #"^p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>$"))]
    (let [[px py pz vx vy vz ax ay az] (->> (rest m) (map utils/parse-int))]
      {:px px :py py :pz pz :vx vx :vy vy :vz vz :ax ax :ay ay :az az})
    (throw (Exception. (format "Could not parse %s" line)))))

(parse-particle "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>")

(defn step [particle]
  (let [particle (-> particle
                     (update :vx (partial + (:ax particle)))
                     (update :vy (partial + (:ay particle)))
                     (update :vz (partial + (:az particle))))]
    (-> particle
        (update :px (partial + (:vx particle)))
        (update :py (partial + (:vy particle)))
        (update :pz (partial + (:vz particle))))))

(defn manhattan-distance [{:keys [px py pz]}]
  (+ (abs px)
     (abs py)
     (abs pz)))

(iterate step (parse-particle "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"))

(->> (utils/read-input "2017/day20.txt")
     (map parse-particle)
     (map (partial iterate step)))

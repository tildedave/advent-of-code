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

(defn answer-part1 []
  (let [long-enough-threshold 1000]
    (->> (utils/read-input "2017/day20.txt")
         (map parse-particle)
         (map (partial iterate step))
         (map-indexed (fn [n state-list] [n (manhattan-distance (nth state-list long-enough-threshold))]))
         (sort-by second)
         (first)
         (first))))



(defn reductive-step [particles]
  (let [next-particles (map step particles)
        positions (map #(select-keys % [:px :py :pz]) next-particles)
        common-positions (->> (frequencies positions)
                              (filter (fn [[_ n]] (> n 1)))
                              (map first)
                              (set))]
    (->> next-particles
         (remove #(contains? common-positions (select-keys % [:px :py :pz]))))))

(defn answer-part2 []
  (->
   (->> (utils/read-input "2017/day20.txt")
        (map parse-particle)
        (iterate reductive-step))
   (nth 10000)
   (count)))

(answer-part2)

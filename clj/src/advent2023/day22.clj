(ns advent2023.day22
  (:require [clojure.set :as set]))

(def example-lines
  ["1,0,1~1,2,1"
   "0,0,2~2,0,2"
   "0,2,3~2,2,3"
   "0,0,4~0,2,4"
   "2,0,5~2,2,5"
   "0,1,6~2,1,6"
   "1,1,8~1,1,9"])

;; directionality is annoying
(defn axis [start end]
  (let [[dx dy dz] (mapv - end start)]
    (cond
      (not= dx 0) [1 0 0]
      (not= dy 0) [0 1 0]
      (not= dz 0) [0 0 1]
      :else (assert false))))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn parse-cube [^String line]
  (let [[left right] (.split line "~")
        parsed-left (mapv Integer/parseInt (.split left ","))
        parsed-right (mapv Integer/parseInt (.split right ","))
        a (axis parsed-left parsed-right)
        idx (case a [1 0 0] 0 [0 1 0] 1 [0 0 1] 2)]
    {:start (if (< (get parsed-left idx) (get parsed-right idx))
              parsed-left
              parsed-right)
     :end (if (< (get parsed-left idx) (get parsed-right idx))
            parsed-right
            parsed-left)
     :axis a}))

(defn parse-cube-with-name [n ^String line]
  (-> (parse-cube line)
      (assoc :name (str (.charAt alphabet n)))))

(assert (= (parse-cube "1,2,1~1,0,1") (parse-cube "1,0,1~1,2,1")))

;; for each brick, fall it down (if it can)
;; how to determine if it can fall down?  well you have the shifted down brick,
;; then see if it intersects the other one.
;; seems like for golang I did just a brute force solution and it worked.

(defn contained? [{:keys [start end]} [x y z]]
  (let [[sx sy sz] start
        [ex ey ez] end]
    (and (<= sx x ex)
         (<= sy y ey)
         (<= sz z ez))))

(assert (contained? (parse-cube "1,0,1~1,2,1") [1 1 1]))

(defn shift-down [{:keys [start end axis] :as brick}]
  (let [[_ _ sz] start [_ _ ez] end]
    (-> brick
        (assoc-in [:start 2] (dec sz))
        (assoc-in [:end 2] (dec ez)))))

(shift-down (parse-cube "1,0,1~1,2,1"))

(defn at-bottom? [{:keys [start]}]
  (= 1 (get start 2)))

(assert (at-bottom? (parse-cube "1,0,1~1,2,1")))
;; (assert (at-bottom? (shift-down (parse-cube "1,0,1~1,2,1"))))

;; OK so the basic data structure is a map of cubes -> which z coords they
;; intersect.
;; then as we shift down, we test the new cube against everything on the "new"
;; plane (this is min of sz, ez of the new cube)

(defn add-to-z-map [acc {:keys [start end _] :as brick}]
  (-> (apply (partial merge-with set/union acc)
             (for [z (range (get start 2) (inc (get end 2)))]
               {z #{brick}}))
      (update :cubes (fnil #(conj % brick) #{}))))

(reduce add-to-z-map {} (map parse-cube example-lines))

(defn shift-in-z-map [z-map cube]
  (if (at-bottom? cube)
    z-map
    (let [shifted (shift-down cube)
          new-z (get-in shifted [:start 2])
          _ (println (disj (get z-map new-z #{}) cube))
        ;;   _ (println "new-z" new-z)
        ;;   _ (println (get z-map new-z #{}))]
          ]
      (if (->> (disj (get z-map new-z #{}) cube)
               (filter #(contained? % (:start shifted)))
               (empty?))
        (do
          (println z-map)
          (println "fall")
          (println (reduce
                    (fn [z-map z]
                      (update z-map z #(-> % (disj cube) (conj shifted))))
                    z-map
                    (range (get (:start cube) 2) (inc (get (:end cube) 2)))))
          (->
           (reduce
            (fn [z-map z]
              (update z-map z #(-> % (disj cube) (conj shifted))))
            z-map
            (range (get (:start cube) 2) (inc (get (:end cube) 2))))
           (update (get-in cube [:end 2]) #(disj % shifted))
           (update new-z (fnil #(conj % shifted) #{}))
           (update :shifted (fnil #(conj % shifted) #{}))
           (update :cubes (fnil #(-> % (conj shifted) (disj cube)) #{}))))
        z-map))))

(let [start-map (reduce add-to-z-map {} (map parse-cube example-lines))]
  (reduce shift-in-z-map start-map (:cubes start-map)))

(defn settle [parsed-cubes]
  (->>
   (iterate
    (fn [z-map]
      (reduce shift-in-z-map (assoc z-map :shifted #{}) (:cubes z-map)))
    (reduce add-to-z-map {} parsed-cubes))
   (take 100)
   (rest)
   (drop-while #(seq (:shifted %)))
   (first)))

(settle (map-indexed parse-cube example-lines))

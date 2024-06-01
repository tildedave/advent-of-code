(ns advent2018.day13
  (:require [utils :as utils]))

(def ^:dynamic part2? false)

(defn parse-track [lines]
  (->> lines
       (map-indexed
        (fn [y line]
          (->> line
               (map-indexed (fn [x ch]
                              (if (not= ch \space)
                                {[x y] ch}
                                nil)))
               (remove nil?)
               (reduce merge))))
       (reduce merge)))

(defn extract-carts [grid]
  [(update-vals grid (fn [ch] (case ch \v \| \^ \| \> \- \< \- ch)))
   (->> grid
        (filter (fn [[coords ch]] (contains? #{\^ \v \> \<} ch)))
        (map (fn [[coords ch]]
               (let [id (random-uuid)]
                 {id {:coords coords :dir ch :num-intersections 0 :crashed? false :id id}})))
        (reduce merge))])

(->> (utils/read-input "2018/day13-example.txt")
     (parse-track)
     (extract-carts))

(defn cart-compare [[x1 y1] [x2 y2]]
  (let [y-comp (compare y1 y2)
        x-comp (compare x1 x2)]
    (case y-comp
      0 x-comp
      -1 y-comp
      1 y-comp)))

(defn next-dir [dir num-intersections]
  (case num-intersections
    0 (case dir \^ \< \< \v \v \> \> \^)
    1 dir
    2 (case dir \^ \> \> \v \v \< \< \^)))

(defn move-cart [grid cart]
  (let [{:keys [coords dir num-intersections]} cart
        [dir num-intersections] (case (grid coords)
                                  \+
                                  [(next-dir dir num-intersections)
                                   (mod (inc num-intersections) 3)]
                                  \\ [(case dir \> \v \^ \< \< \^ \v \>) num-intersections]
                                  \/ [(case dir \v \< \> \^ \< \v \^ \>) num-intersections]
                                  [dir num-intersections])
        [dx dy] (case dir
                      \^ [0 -1]
                      \v [0 1]
                      \> [1 0]
                      \< [-1 0])]
    (-> cart
        (update :coords (fn [[x y]] [(+ x dx) (+ y dy)]))
        (assoc :dir dir)
        (assoc :num-intersections num-intersections))))

(defn tick [grid carts]
    ;; for each cart, step it forward
    ;; if a cart is at a "+", then we apply the num to determine
    ;; new direction (and make the step from there).
  (reduce
   (fn [carts cart]
     (if (:crashed? cart)
       carts
       (let [next-cart (move-cart grid cart)
             crash-carts (filter (fn [{:keys [coords id]}] (and
                                                         (not= (:id next-cart) id)
                                                         (= (:coords next-cart) coords)))
                                 (vals carts))]
         (if (empty? crash-carts)
           (assoc carts (:id next-cart) next-cart)
           (if part2?
             (reduce
              dissoc
              (dissoc carts (:id next-cart))
              (map :id crash-carts))
             (reduce
              (fn [carts id] (assoc-in carts [id :crashed?] true))
              (assoc carts (:id next-cart) (assoc next-cart :crashed? true))
              (map :id crash-carts)))))))
   carts
   (sort-by :coords cart-compare (vals carts))))

;; part1
(let [[grid carts] (->> (utils/read-input "2018/day13.txt")
                        (parse-track)
                        (extract-carts))]
  (reduce
   (fn [acc carts]
     (if-let [x (first (filter :crashed? carts))]
       (reduced (:coords x))
       acc))
   nil
   (map vals (iterate (partial tick grid) carts))))

(binding [part2? true]
  (let [[grid carts] (->> (utils/read-input "2018/day13.txt")
                          (parse-track)
                          (extract-carts))]
    (reduce
     (fn [acc carts]
       (case (count carts)
         1 (reduced (:coords (first carts)))
         acc))
     nil
     (map vals (iterate #(tick grid %) carts))))
)

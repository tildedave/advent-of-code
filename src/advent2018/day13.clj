(ns advent2018.day13
  (:require [utils :as utils]))

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
        (map (fn [[coords ch]] {:coords coords :dir ch :num-intersections 0 :crashed? false})))])

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

(defn tick [grid]
  (fn [carts]
    ;; for each cart, step it forward
    ;; if a cart is at a "+", then we apply the num to determine
    ;; new direction (and make the step from there).
    (reduce
     (fn [carts cart]
       (if (:crashed? cart)
         (conj carts cart)
         (let [next-cart (move-cart grid cart)
               crash-carts (filter (fn [{:keys [coords]}] (= (:coords next-cart) coords)) carts)]
           (if (empty? crash-carts)
             (conj carts next-cart)
             (conj (map (fn [cart] (if (= (:coords cart) (:coords next-cart))
                                     (assoc cart :crashed? true)
                                     cart)) carts)
                   (assoc next-cart :crashed? true))))))
     []
     (sort-by :coords cart-compare carts))))

(let [[grid carts] (->> (utils/read-input "2018/day13.txt")
                        (parse-track)
                        (extract-carts))]
  (reduce
   (fn [acc carts]
     (if-let [x (first (filter :crashed? carts))]
       (reduced (:coords x))
       acc))
   nil
   (take 100 (iterate (tick grid) carts))))

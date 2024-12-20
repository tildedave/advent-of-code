(ns advent2024.day20
  (:require
   [grid :as grid]
   [graph :as graph]
   [utils :as utils]))

(defn char-position [grid ch]
  (->> (grid/coords grid) (filter #(= (grid/at grid %) ch)) (first)))

(defn start-coords [grid] (char-position grid \S))
(defn end-coords [grid] (char-position grid \E))

(grid/parse-file "2024/day20-example.txt")

;; dijsktra kind of sucks
;; (defn search [grid]
;;   (let [end-coords (end-coords grid)
;;         distances (graph/dijkstra-search
;;                    {:coords (start-coords grid) :cheat nil}
;;                    (fn [{:keys [coords cheat] :as p}]
;;                      (->> grid/cardinal-directions
;;                           (map (fn [d]
;;                                  (let [next (mapv + coords d)
;;                                        is-wall? (= (grid/at grid next) \#)]
;;                                    (cond
;;                                      (and (nil? cheat) is-wall?)
;;                                      {:coords (mapv + next d)
;;                                       :cheat coords}
;;                                      is-wall? nil
;;                                      :else (assoc p :coords next)))))
;;                           (remove nil?)))
;;                    (fn [& _] false)
;;                    (fn [current neighbor]
;;                      (utils/manhattan-distance
;;                       (:coords current)
;;                       (:coords neighbor))))
;;         no-cheat-distance (distances {:coords  end-coords :cheat nil})]
;;     [(->> distances
;;          (filter (fn [[k _]] (= end-coords (:coords k))))

;;          )]))

;; OK we'll try something that might be invalidated by p2
;; I guess we might "cheat" into a square that we haven't gotten to
;; so a*-search

(defn reconstruct [distances start-coords]
  (loop [curr start-coords
         path []]
    (let [d (distances curr)]
      (if (zero? d)
        path
        (recur
         (->> grid/cardinal-directions
             (map #(mapv + curr %))
             (filter #(= (dec d) (distances %)))
             (first))
         (conj path curr))))))

(defn search [grid]
  (let [end-coords (end-coords grid)
        distances
        (graph/dijkstra-search end-coords
                               (fn [c] (grid/neighbors grid c grid/cardinal-directions #(= % \#)))
                               (fn [& _] false))
        path (reconstruct distances (start-coords grid))]
    (->> path
         (mapcat
          (fn [c]
            (->> grid/cardinal-directions
                 (filter (fn [d] (= (grid/at grid (mapv + d c)) \#)))
                 (map #(mapv + % % c))
                 (remove #(nil? (distances %)))
                 (map (fn [cheat-dest]
                        (- (distances c)
                           (distances cheat-dest)
                           2)))
                 (remove #(< % 0))
                 ))))))

(defn answer-part1 [grid]
  (let [saves (search grid)]
    (count (filter #(>= % 100) saves))))

(answer-part1 (grid/parse-file "2024/day20.txt"))

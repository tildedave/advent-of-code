(ns advent2019.day18
  (:require [grid :as grid]
            [clojure.test :refer [deftest is run-tests testing]]
            [graph :as graph]
            [clojure.set :as set]))

;; OK so what we have is some kind of search, and we care about the shortest
;; distances between all nodes because that's what lets us walk between them.

(def example-grid
  '("#########"
    "#b.A.@.a#"
    "#########"))

(grid/parse example-grid)

(defn starting-positions [grid]
  (->> (grid/coords grid)
       (filterv #(= (grid/at grid %) \@))))

(starting-positions (grid/parse example-grid))

(defn graph-nodes [grid]
  (->> (grid/coords grid)
       (filter #(Character/isAlphabetic (int (grid/at grid %))))
       (map #(hash-map (grid/at grid %) %))
       (into {})))

(defn key-nodes [grid]
  (select-keys (graph-nodes grid) (set '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z))))

(key-nodes (grid/parse example-grid))

;; so I guess the most realistic approach here is
;; Dijkstra from each node, which sucks.
;; but: whatever.

(defn adjacencies [grid]
  (let [graph-nodes (vals (graph-nodes grid))]
    (into {}
          (for [node (concat (starting-positions grid) graph-nodes)]
            [node
     ;; this is the distance but we want to filter the keys
             (-> (graph/dijkstra-search node
                                        #(grid/neighbors
                                          grid
                                          %
                                          grid/cardinal-directions
                                          (fn [ch] (= ch \#)))
                                        (fn [current _]
                                    ;; we stop at any key or door.
                                          (let [ch (grid/at grid current)]
                                            (and (not= ch \.)
                                                 (not= ch \@)
                                                 (not= current node)))))
                 (select-keys graph-nodes)
                 (dissoc node))]))))

(def example-multigrid1 '("###############"
                          "#d.ABC.#.....a#"
                          "######@#@######"
                          "###############"
                          "######@#@######"
                          "#b.....#.....c#"
                          "###############"))

(adjacencies (grid/parse example-grid))
(adjacencies (grid/parse-file "2019/day18.txt"))
(adjacencies (grid/parse example-multigrid1))
;; so, I guess we'll use the A* algo.

(defn is-key? [ch]
  (Character/isLowerCase (int ch)))

(defn is-door? [ch]
  (Character/isUpperCase (int ch)))

(defn door-key [ch]
  (Character/toLowerCase ch))


(adjacencies (grid/parse example-grid))
(door-key \C)

(defn state-hash [state] (dissoc state :new-key :distance))

(defn is-strictly-better
  "Is node2 strictly better than node1?"
  [goal-score node1 node1-score node2]
  (let [node2-score (goal-score (state-hash node2))]
    (and (<= node1-score node2-score)
         (set/subset? (:collected-keys node1)
                      (:collected-keys node2))
         (not= (:collected-keys node1)
               (:collected-keys node2)))))

(defn reachable-keys [grid adjacencies start-pos]
  (loop [queue [start-pos]
         seen #{}
         result #{}]
    (if-let [x (first queue)]
      (let [at-x (grid/at grid x)]
        (recur
         (reduce
          (fn [queue node]
            (if (contains? seen node)
              queue
              (conj queue node)))
          (subvec queue 1)
          (keys (adjacencies x)))
         (conj seen x) (if (is-key? at-x)
                         (conj result at-x)
                         result)))
      result)))

(reachable-keys
 (grid/parse example-grid)
 (adjacencies (grid/parse example-grid))
 (first (starting-positions (grid/parse example-grid))))

;; needed: cutoff where we stop moving after we have all keys in a quadrant.
;; start pos -> adjacencies of it -> some recursive logic for this -> ugh
(defn minimum-steps [grid]
  (let [adjacencies (adjacencies grid)
        all-keys (set (keys (key-nodes grid)))
        best-for-position (atom {})
        reachable-by-idx (->> (starting-positions grid)
                              (map-indexed vector)
                              (map (fn [[n x]] {n (reachable-keys grid adjacencies x)}))
                              (into {}))]
    (graph/a*-search
     {:coords (starting-positions grid) :collected-keys #{}}
     (fn [{:keys [collected-keys]}] (set/subset? all-keys collected-keys))
     (fn [state]
       (let [{:keys [coords collected-keys]} state]
       ;; coords is a vector of each actor involved
       ;; for each actor, we want to move it (and only it)
         (->> coords
            ;; for each neighbor, can we reach it?
            ;; we can reach it if it's uppercase AND we have the key.
            ;; possibly wrong: there is no reason to go back to a key we already have.
            ;; if we go to a location with a key, we need to
              (map-indexed vector)
              (mapcat
               (fn [[n actor-coords]]
                 (if (set/superset? collected-keys (reachable-by-idx n))
                   nil
                   (->> (adjacencies actor-coords)
                        (map
                         (fn [[dest dist]]
                           (let [at-coord (grid/at grid dest)]
                             (cond
                               (is-key? at-coord)
                               {:coords (assoc coords n dest)
                                :collected-keys (conj collected-keys at-coord)
                                :new-key (if (contains? collected-keys at-coord)
                                           nil
                                           at-coord)
                                :distance dist}
                               (is-door? at-coord)
                               (if (contains? collected-keys (door-key at-coord))
                                 {:coords (assoc coords n dest)
                                  :collected-keys collected-keys
                                  :distance dist}
                                 nil)
                       ;; we need to head back to the @ at various points
                               :else {:coords (assoc coords n dest)
                                      :collected-keys collected-keys
                                      :distance dist}))))
                        (remove nil?))))))))
     ;; lower heuristic is better
     (fn [{:keys [collected-keys new-key]}]
       (+ (- (count all-keys) (count collected-keys))
          (if (nil? new-key) 100 0)))
     (fn [_ {:keys [distance]}] distance)
     state-hash
     ;; OK cutoff logic.
     ;; when we get a node to check if it's cutoff time,
     ;; we check our atom for its coords.
     ;; if the coords are in our atom, we check the goal score.
     ;; OK we also need to use state-hash for the goal-score lookup.
     ;; current goal has a score as well via the goal-score.
     (fn [current goal-score]
       (let [current-score (goal-score (state-hash current))
             nodes-only (:coords current)
             best-nodes (get @best-for-position nodes-only [])
            ;;  _ (if (> (count best-nodes) 100) (println "the best are"
            ;;                                            (map #(vector % (goal-score (state-hash %))) best-nodes)))
             better-node (->> best-nodes
                              (filter (partial is-strictly-better goal-score current current-score))
                              (first))]
         (if (nil? better-node)
           (do
             (swap! best-for-position #(assoc % nodes-only (conj best-nodes current)))
             false)
           (let [better-score (goal-score (state-hash better-node))]
            ;;    (println
            ;;     (format "cutting off %s (%d) vs %s (%d)"
            ;;             current
            ;;             (goal-score (state-hash current))
            ;;             better-node
            ;;             (goal-score (state-hash better-node))))
             ;; we'll use this opportunity to prune the best list
             (swap! best-for-position
                    #(update % nodes-only
                             (fn [other-nodes]
                               (remove (fn [other-node]
                                         (is-strictly-better goal-score other-node (goal-score (state-hash other-node)) better-node))
                                       other-nodes))))
             true)))))))
    ;; )))

(def example-grid2
  '("########################"
    "#f.D.E.e.C.b.A.@.a.B.c.#"
    "######################.#"
    "#d.....................#"
    "########################"))

(def example-grid3
  '("########################"
    "#...............b.C.D.f#"
    "#.######################"
    "#.....@.a.B.c.d.A.e.F.g#"
    "########################"))

(def example-grid4
  '("#################"
    "#i.G..c...e..H.p#"
    "########.########"
    "#j.A..b...f..D.o#"
    "########@########"
    "#k.E..a...g..B.n#"
    "########.########"
    "#l.F..d...h..C.m#"
    "#################"))

(def example-grid5
  '("########################"
    "#@..............ac.GI.b#"
    "###d#e#f################"
    "###A#B#C################"
    "###g#h#i################"
    "########################"))

(deftest examples
  (is (= (first (minimum-steps (grid/parse example-grid))) 8))
  (is (= (first (minimum-steps (grid/parse example-grid2))) 86))
  (is (= (first (minimum-steps (grid/parse example-grid3))) 132))
  (is (= (first (minimum-steps (grid/parse example-grid4))) 136))
  (is (= (first (minimum-steps (grid/parse example-grid5))) 81)))

(time (println (minimum-steps (grid/parse example-grid2))))
(time (println (minimum-steps (grid/parse example-grid3))))
(time (println (minimum-steps (grid/parse example-grid4))))
(time (println (minimum-steps (grid/parse example-grid5))))

(run-tests 'advent2019.day18)

;; part 1
(println (minimum-steps (grid/parse-file "2019/day18.txt")))

;; so part 2 is the same except the grid "neighbors" are a little more
;; complicated, and we may need to prune?

(def example-multigrid2 '("#############"
                          "#DcBa.#.GhKl#"
                          "#.###@#@#I###"
                          "#e#d#####j#k#"
                          "###C#@#@###J#"
                          "#fEbA.#.FgHi#"
                          "#############"))

(def example-multigrid3 '("#############"
                          "#g#f.D#..h#l#"
                          "#F###e#E###.#"
                          "#dCba@#@BcIJ#"
                          "#############"
                          "#nK.L@#@G...#"
                          "#M###N#H###.#"
                          "#o#m..#i#jk.#"
                          "#############"))

(deftest multigrid-examples
  (is (= (first (minimum-steps (grid/parse example-multigrid1))) 24))
  (is (= (first (minimum-steps (grid/parse example-multigrid2))) 32))
  (is (= (first (minimum-steps (grid/parse example-multigrid3))) 72)))

(reachable-keys
 (grid/parse example-grid)
 (adjacencies (grid/parse example-grid))
 (first (starting-positions (grid/parse example-grid))))

(run-tests 'advent2019.day18)
grid/all-directions

(defn convert-to-multigrid [grid]
  (let [[[x y]] (starting-positions grid)]
    (as-> grid g
      (reduce
       (fn [grid [x y]] (assoc-in grid [y x] \@))
       g
       (for [[dx dy] grid/ordinal-directions]
         [(+ x dx) (+ y dy)]))
      (reduce
       (fn [grid [x y]] (assoc-in grid [y x] \#))
       g
       (for [[dx dy] grid/cardinal-directions]
         [(+ x dx) (+ y dy)])))))

;; OK so we need a cutoff mechanism.
;; with a cutoff mechanism we can remember every state and associate a distance
;; to it (via an atom).
(time (println (minimum-steps (convert-to-multigrid (grid/parse-file "2019/day18.txt")))))

(ns advent2019.day18
  (:require [grid :as grid]
            [graph :as graph]
            [clojure.set :as set]))

;; OK so what we have is some kind of search, and we care about the shortest
;; distances between all nodes because that's what lets us walk between them.

(def example-grid
  '("#########"
    "#b.A.@.a#"
    "#########"))

(grid/parse example-grid)

(defn starting-position [grid]
  (->> (grid/coords grid)
       (filter #(= (grid/at grid %) \@))
       (first)))

(starting-position (grid/parse example-grid))

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
          (for [node (cons (starting-position grid) graph-nodes)]
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

(adjacencies (grid/parse example-grid))
(adjacencies (grid/parse-file "2019/day18.txt"))

;; so, I guess we'll use the A* algo.

(defn is-key? [ch]
  (Character/isLowerCase (int ch)))

(defn is-door? [ch]
  (Character/isUpperCase (int ch)))

(defn door-key [ch]
  (Character/toLowerCase ch))


(adjacencies (grid/parse example-grid))
(door-key \C)

(defn minimum-steps [grid]
  (let [adjacencies (adjacencies grid)
        all-keys (set (keys (key-nodes grid)))]
    (graph/a*-search
     {:coords (starting-position grid) :collected-keys #{}}
     (fn [{:keys [collected-keys]}] (set/subset? all-keys collected-keys))
     (fn [{:keys [coords collected-keys]}]
       ;; so we find the neighbors of our coords, then we filter them.
       (->> (adjacencies coords)
            ;; for each coord, can we reach it?
            ;; we can reach it if it's uppercase AND we have the key.
            ;; possibly wrong: there is no reason to go back to a key we already have.
            ;; if we go to a location with a key, we need to
            (map
             (fn [[dest dist]]
               (let [at-coord (grid/at grid dest)]
                 (cond
                   (is-key? at-coord)
                   {:coords dest
                    :collected-keys (conj collected-keys at-coord)
                    :distance dist}
                   (is-door? at-coord)
                   (if (contains? collected-keys (door-key at-coord))
                     {:coords dest
                      :collected-keys collected-keys
                      :distance dist}
                     nil)
                       ;; we need to head back to the @ at various points
                   :else {:coords dest
                          :collected-keys collected-keys
                          :distance dist}))))
            (remove nil?)))
     ;; lower heuristic is better
     (fn [{:keys [collected-keys]}]
       (- (count all-keys) (count collected-keys)))
     (fn [_ {:keys [distance]}] distance))))

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

(minimum-steps (grid/parse example-grid))
(minimum-steps (grid/parse example-grid2))
(minimum-steps (grid/parse example-grid3))
(minimum-steps (grid/parse example-grid4))
(minimum-steps (grid/parse example-grid5))

;; answer is incorrect.  gives me 4686 (and then 3866 after I change the
;; heuristic).  actual answer is 3688.
(minimum-steps (grid/parse-file "2019/day18.txt"))

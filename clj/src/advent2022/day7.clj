(ns advent2022.day7
  (:require [utils :as utils]))

(def lines (utils/read-resource-lines "input/day7.txt"))

(defn process-line [fs lines current-path]
  (let [line (first lines)]
    (cond
      (= line "$ cd /") [fs (rest lines) []]
      (= line "$ cd ..") [fs (rest lines) (pop current-path)]
      (= line "$ ls")
    ;; must grab lines until we see something starting with "$"
      (loop [fs (utils/dissoc-in fs current-path)
             lines (rest lines)]
        (if (empty? lines) [fs lines current-path]
            (let [line (first lines)]
              (cond
            ;; we're done
                (.startsWith line "$") [fs lines current-path]
                (.startsWith line "dir")
                (let [dir-name (nth (.split line " ") 1)]
                  (recur (assoc-in fs (conj current-path dir-name) ["dir"]) (rest lines)))
                :else
            ;; it's a file, parse the first line, add it to the tree, and recur
                (let [[num-str file-name] (.split line " ")]
                  (recur (assoc-in fs (conj current-path file-name) ["file" (utils/parse-int num-str)]) (rest lines)))))))
        (.startsWith line "$ cd") [fs (rest lines) (conj current-path (nth (.split line "cd ") 1))]
         ;; we don't handle this case yet
         :else (throw (Exception. (format "unhandled case: %s" line))))))

;;  ["a" "e"]
;;  ["dir"])

(def file-structure
  (loop [[fs lines current-path] [nil lines nil]]
    (if (empty? lines) fs
        (recur (process-line fs lines current-path)))))

;; there's certainly a better way to do this
;; this also isn't tail recursive >:-(
(defn calculate-size
  [fs current-path reporter]
  ;; we need to report [path size-of-path]
  (let [current-fs (if (empty? current-path) fs (get-in fs current-path))
        grouped (group-by #(vector? (current-fs %)) (keys current-fs))
        [files directories] [(grouped true) (grouped false)]
        file-sizes (reduce + 0 (map #(if (= (nth (current-fs %) 0) "file") (nth (current-fs %) 1) 0) files))
        directory-sizes (reduce + 0 (map #(calculate-size fs (conj current-path %) reporter) directories))
        result (+ file-sizes directory-sizes)]
    (do
     (reporter result)
     result)))

;; part 1 answer
(let [answer (atom 0)]
  (calculate-size file-structure [] (fn [num] (if (< num 100000) (swap! answer + num) nil)))
  (deref answer))

;; part 2 answer
(let [answer (atom Integer/MAX_VALUE)
      total-disk-space 70000000
      needed-unused-space 30000000
      current-used-space (calculate-size file-structure [] identity)
      current-free-space (- total-disk-space current-used-space)]
  (calculate-size file-structure []
                  (fn [num]
                    (let [remaining-free-space (- (+ current-free-space num) needed-unused-space)]
                      (swap! answer (fn [curr free]
                                      (if (> free 0) (min curr num) curr)) remaining-free-space))))
  (deref answer))

;; atom was kind of ugly but made part 2 easier.

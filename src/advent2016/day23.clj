(ns advent2016.day23
  (:require [utils :as utils]
            [advent2016.assembunny :as assembunny]))


(assembunny/full-exec "2016/day23-example.txt")
(assembunny/full-exec "2016/day23.txt" {"a" 7})

;; OK so this thing is computing factorials
;; and it looks like it only runs the toggle 5 times.
;; after the toggles run, a has 7! in its register
;; then we add 81 * 73 to it.

(->> (assembunny/program-seq "2016/day23.txt" {"a" 7})
     (map (fn [[program n state]] [(get program n) n state]))
     (map-indexed vector)
     (drop 139000))

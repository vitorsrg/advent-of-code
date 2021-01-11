;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 3: Toboggan Trajectory
;; url:       https://adventofcode.com/2020/day/3
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2020-12-30
;; execution: $ bash ./aoc2020/run.sh d03 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d03 < ./aoc2020/d03/input.txt
;;            part 1 276
;;            part 2 7812180000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d03.main
    ;; (:require [clojure.core.matrix])
)

(defn parse-map-line [line] (nth (re-find #"(\S+)" line) 1))

(defn tree-map-path-tree-count
  [tree-map [right-step down-step]]
  (let [width (count (get tree-map 0))
        height (count tree-map)
        path (for [i (range 0 height down-step)
                   :let [j (mod (* (/ i down-step) right-step) width)]]
               [i j])
        path-content (map #(get-in tree-map %) path)
        path-tree-count (count (filter #(= \# %) path-content))]
    path-tree-count))

(defn -main
  [& args]
  (let [input-file-name *in*
        input-lines (clojure.string/split-lines (slurp input-file-name))
        tree-map (vec (map parse-map-line input-lines))]
    ;; part 1
    (let [path-tree-count (tree-map-path-tree-count tree-map [3 1])]
      (println "part 1" path-tree-count))
    ;; part 2
    (let [path-slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]
          path-slopes-tree-counts
            (zipmap path-slopes
                    (map #(tree-map-path-tree-count tree-map %) path-slopes))
          argmin-path-slope-tree-count
            (apply min-key #(get path-slopes-tree-counts %) path-slopes)
          path-slopes-tree-count-product (apply *
                                           (vals path-slopes-tree-counts))]
      (println "part 2" path-slopes-tree-count-product))))

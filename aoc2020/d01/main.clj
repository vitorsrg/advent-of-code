;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 1: Report Repair
;; url:       https://adventofcode.com/2020/day/1
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2020-12-28
;; execution: bash ./aoc2020/run.sh d01
;; example:
;;            bash ./aoc2020/run.sh d01
;;            part 1 646779
;;            part 2 246191688
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d01.main
  (:require [clojure.set])
  (:require [clojure.string]))

(defn -main [& args]
  (let [input-file-name (first args)
        sum-target 2020
        entries-list (map #(Integer/parseInt %)
                       (clojure.string/split-lines (slurp input-file-name)))]
    ;; part 1
    (let [queries-list (map #(- 2020 %) entries-list)
          matches (clojure.set/intersection (set entries-list)
                                            (set queries-list))
          entry1 (first matches)
          entry2 (- 2020 entry1)
          pair-mul (* entry1 entry2)]
      (println "part 1" pair-mul))
    ;; part 2
    (let [entries-set (set entries-list)
          entries-count (count entries-list)
          matcher (for [i (range entries-count)
                        j (range (+ i 1) entries-count)
                        :let [x (nth entries-list i)
                              y (nth entries-list j)
                              z (- 2020 x y)]
                        :when (contains? entries-set z)]
                    [x y z])
          triplet (first matcher)
          triplet-mul (apply * triplet)]
      (println "part 2" triplet-mul))))


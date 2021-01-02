;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day XX:
;; url:       https://adventofcode.com/2020/day/XX
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      yyyy-MM-dd
;; execution: bash ./aoc2020/run.sh dXX
;; example:
;;            bash ./aoc2020/run.sh dXX
;;            part 1
;;            part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns dXX.main)

(defn -main
  [& args]
  (let [input-file-name (first args)
        input-lines (clojure.string/split-lines (slurp input-file-name))]
    ;; part 1
    (let [] (println "part 1"))
    ;; part 2
    (let [] (println "part 2"))))

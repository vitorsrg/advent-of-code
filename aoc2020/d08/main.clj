;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 8: Handheld Halting
;; url:       https://adventofcode.com/2020/day/8
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-06
;; execution: $ bash ./aoc2020/run.sh d08
;; example:
;;            $ bash ./aoc2020/run.sh d08
;;            part 1 1753
;;            part 2 733
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d08.main)

(defn parse-instruction
  [instruction-raw]
  (let [[_ func arg] (re-matches #"^(\w+) ([+-]\d+)$" instruction-raw)]
    [(keyword func) (Integer/parseInt arg)]))

(defn run-only-once
  [instructions pointer accumulator already-run]
  (cond (>= pointer (count instructions)) [pointer accumulator already-run
                                           :halt]
        (contains? already-run pointer) [pointer accumulator already-run :loop]
        :else (let [[func arg] (nth instructions pointer)]
                (case func
                  :nop (recur instructions
                              (inc pointer)
                              accumulator
                              (conj already-run pointer))
                  :acc (recur instructions
                              (inc pointer)
                              (+ accumulator arg)
                              (conj already-run pointer))
                  :jmp (recur instructions
                              (+ pointer arg)
                              accumulator
                              (conj already-run pointer))))))

(defn brute-force-halting
  [instructions]
  (->>
    (for [i (range (count instructions))
          :let [[func arg] (nth instructions i)]]
      (case func
        :nop (let [result
                     (run-only-once (assoc instructions i [:jmp arg]) 0 0 #{})]
               (if (= (nth result 3) :halt) result))
        :jmp (let [result
                     (run-only-once (assoc instructions i [:nop arg]) 0 0 #{})]
               (if (= (nth result 3) :halt) result))
        nil))
    (filter some?)
    (first)))

(defn -main
  [& args]
  (let [instructions (->> (first args)
                          (slurp)
                          (clojure.string/split-lines)
                          (map parse-instruction)
                          (vec))]
    ;; (clojure.pprint/pprint instructions)
    ;; part 1
    (let [[_ accumulator _] (run-only-once instructions 0 0 #{})]
      (println "part 1" accumulator))
    ;; part 2
    ;; ideally this could be done in linear time with a dfs, but this quadratic
    ;; solution is reasonably fast for this problem size
    (let [[_ accumulator _] (brute-force-halting instructions)]
      (println "part 2" accumulator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 2: Password Philosophy
;; url:       https://adventofcode.com/2020/day/2
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2020-12-29
;; execution: $ lein run -m main
;; example:
;;            $ lein run -m main
;;            part 1 416
;;            part 2 688
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns main
  (:require [clojure.set])
  (:require [clojure.string]))

(defn p1-parse-line [line]
  (let [[minc maxc letter password]
        (rest (re-find #"(\d+)-(\d+)\s+(\w)\:\s(\w+)" line))]
    {:minc (Integer/parseInt minc)
     :maxc (Integer/parseInt maxc)
     :letter (nth letter 0)
     :password password}))

(defn p1-is-row-valid? [{:keys [minc maxc letter password]}]
  (let [frequency (get (frequencies password) letter 0)]
    (and (>= frequency minc) (<= frequency maxc))))

(defn p2-parse-line [line]
  (let [[i1 i2 letter password]
        (rest (re-find #"(\d+)-(\d+)\s+(\w)\:\s(\w+)" line))]
    {:i1 (Integer/parseInt i1)
     :i2 (Integer/parseInt i2)
     :letter (nth letter 0)
     :password password}))

(defn p2-is-row-valid? [{:keys [i1 i2 letter password]}]
  (let [letter-eqs-i1 (= letter (nth password (- i1 1) nil))
        letter-eqs-i2 (= letter (nth password (- i2 1) nil))]
    (and
     (or letter-eqs-i1 letter-eqs-i2)
     (or (not letter-eqs-i1) (not letter-eqs-i2)))))

(defn -main [& args]

  (let [input-file-name "./input.txt"
        input-lines (clojure.string/split-lines (slurp input-file-name))]

    ;; part 1
    (let [input-rows (map p1-parse-line input-lines)
          input-row-is-valid-seq (map p1-is-row-valid? input-rows)
          input-row-is-valid-count (count (filter identity input-row-is-valid-seq))]
      (println "part 1" input-row-is-valid-count))

    ;; part 2
    (let [input-rows (map p2-parse-line input-lines)
          input-row-is-valid-seq (map p2-is-row-valid? input-rows)
          input-row-is-valid-count (count (filter identity input-row-is-valid-seq))]
      (println "part 2" input-row-is-valid-count))))

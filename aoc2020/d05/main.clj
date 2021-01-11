;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 5: Binary Boarding
;; url:       https://adventofcode.com/2020/day/5
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-02
;; execution: $ bash ./aoc2020/run.sh d05 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d05 < ./aoc2020/d05/input.txt
;;            part 1 864
;;            part 2 739
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d05.main)

(defn replace-several
  [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(apply clojure.string/replace %1 %2) content replacement-list)))

(defn parse-seat
  [seat-code]
  (let [[seat-code row-code col-code] (re-find #"([FB]{7})([LR]{3})" seat-code)
        row-bincode (replace-several row-code "F" "0" "B" "1")
        col-bincode (replace-several col-code "L" "0" "R" "1")
        row-number (Integer/parseInt row-bincode 2)
        col-number (Integer/parseInt col-bincode 2)
        seat-id (+ (* row-number 8) col-number)]
    {:seat-code seat-code,
     ;; :row-code row-code,
     ;; :col-code col-code,
     ;; :row-bincode row-bincode,
     ;; :col-bincode col-bincode,
     :row-number row-number,
     :col-number col-number,
     :seat-id seat-id}))

(defn -main
  [& args]
  (let [seats (->> *in*
                   (slurp)
                   (clojure.string/split-lines)
                   (map parse-seat)
                   (vec))]
    ;; (run! println (map parse-seat ["BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"]))
    ;; part 1
    (let [max-seat-id (apply max (map :seat-id seats))]
      (println "part 1" max-seat-id))
    ;; part 2
    (let [seat-ids-sorted (vec (sort (map :seat-id seats)))
          missing-seat-id
            (first (for [i (range (- (count seat-ids-sorted) 1))
                         :let [seat-id (seat-ids-sorted i)
                               next-seat-id (seat-ids-sorted (+ i 1))]
                         :when (= next-seat-id (+ seat-id 2))]
                     (+ seat-id 1)))]
      (println "part 2" missing-seat-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 10: Adapter Array
;; url:       https://adventofcode.com/2020/day/10
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-07
;; execution: $ bash ./aoc2020/run.sh d10 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d10 d10/ex01.txt
;;            part 1 35
;;            part 2 8
;;            $ bash ./aoc2020/run.sh d10 d10/ex01.txt
;;            part 1 220
;;            part 2 19208
;;            $ bash ./aoc2020/run.sh d10
;;            part 1 1980
;;            part 2 4628074479616
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d10.main)

(defn count-adapter-arrangements
  [adapters-asc]
  (let [adapters-set (set adapters-asc)
        search-fn (fn [search-fn ni]
                    (if (= ni 0)
                      1
                      (+ (if (contains? adapters-set (- ni 1))
                           (search-fn search-fn (- ni 1))
                           0)
                         (if (contains? adapters-set (- ni 2))
                           (search-fn search-fn (- ni 2))
                           0)
                         (if (contains? adapters-set (- ni 3))
                           (search-fn search-fn (- ni 3))
                           0))))
        memo-search-fn (memoize search-fn)
        adapters-max (apply max adapters-asc)]
    (memo-search-fn memo-search-fn adapters-max)))

(defn -main
  [& args]
  (let [numbers (->> (first args)
                     (slurp)
                     (clojure.string/split-lines)
                     (map #(Integer/parseInt %))
                     (vec))
        adapters-asc (-> numbers
                         (conj (+ (apply max numbers) 3) 0)
                         (sort)
                         (vec))]
    ;; (clojure.pprint/pprint [(count numbers) (count adapters-asc)])
    ;; part 1
    (let [adapters-asc-diffs (for [i (range (dec (count adapters-asc)))
                                   :let [ni (nth adapters-asc i)
                                         nj (nth adapters-asc (inc i))]]
                               (- nj ni))
          adapters-asc-diffs-freqs (frequencies adapters-asc-diffs)]
      (println "part 1"
               (* (get adapters-asc-diffs-freqs 1)
                  (get adapters-asc-diffs-freqs 3))))
    ;; part 2
    (println "part 2" (count-adapter-arrangements adapters-asc))))

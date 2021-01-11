;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 9: Encoding Error
;; url:       https://adventofcode.com/2020/day/9
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-07
;; execution: $ bash ./aoc2020/run.sh d09 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d09 < ./aoc2020/d09/input.txt
;;            part 1
;;            part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d09.main)

(defn dec-dissoc [m k] (if (= (get m k) 1) (dissoc m k) (update m k dec)))

(defn simulate-valid-xmas
  ([numbers pool-max-size] (simulate-valid-xmas numbers pool-max-size 0 {}))
  ([numbers pool-max-size i pool]
   (let [ni (nth numbers i)
         pool-size (count pool)]
     (if (< pool-size pool-max-size)
       (lazy-seq (cons [i ni pool-size nil]
                       (simulate-valid-xmas numbers
                                            pool-max-size
                                            (inc i)
                                            (update pool ni (fnil inc 0)))))
       (let [pair (first (for [x (keys pool)
                               :let [y (- ni x)]
                               :when (contains? pool y)]
                           [x y]))]
         (if (some? pair)
           (lazy-seq
             (cons [i ni pool-size pair]
                   (simulate-valid-xmas
                     numbers
                     pool-max-size
                     (inc i)
                     (update (dec-dissoc pool (nth numbers (- i pool-size)))
                             ni
                             (fnil inc 0)))))))))))

(defn contiguous-subsequence-by-sum
  [arr target lo hi sum]
  (cond (= sum target) [lo hi]
        (> sum target) (recur arr target (inc lo) hi (- sum (nth arr lo)))
        (< sum target) (if (< hi (count arr))
                         (recur arr target lo (inc hi) (+ sum (nth arr hi))))))

(defn -main
  [& args]
  (let [numbers (->> *in*
                     (slurp)
                     (clojure.string/split-lines)
                     (map bigint)
                     (vec))
        simulated-valid-xmas (vec (simulate-valid-xmas numbers 25))
        first-not-valid-xmas-idx (count simulated-valid-xmas)]
    ;; (clojure.pprint/pprint numbers)
    ;; (clojure.pprint/pprint simulated-valid-xmas)
    ;; part 1
    (println "part 1" (biginteger (nth numbers first-not-valid-xmas-idx)))
    ;; part 2
    (let [[lo hi] (contiguous-subsequence-by-sum
                    (subvec numbers 0 first-not-valid-xmas-idx)
                    (nth numbers first-not-valid-xmas-idx)
                    0
                    0
                    0)
          numbers-xmas-csubseq (subvec numbers lo hi)]
      (println "part 2"
               (biginteger (+ (apply min numbers-xmas-csubseq)
                              (apply max numbers-xmas-csubseq)))))))

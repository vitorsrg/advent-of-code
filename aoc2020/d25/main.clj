;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 25: Combo Breaker
;; url:       https://adventofcode.com/2020/day/25
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-06-01
;; execution: $ bash ./aoc2020/run.sh d25 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d25 < ./aoc2020/d25/ex01.txt
;;            part 1 14897079
;;            $ bash ./aoc2020/run.sh d25 < ./aoc2020/d25/input.txt
;;            part 1 11328376
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d25.main
  (:require [clojure.math.numeric-tower]))

(defn mod-pow
  [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m))
  ;; (loop [b (mod b m)
  ;;        e e
  ;;        r 1]
  ;;   (if (> e 0)
  ;;     (recur ;
  ;;       (mod (* b b) m) ;
  ;;       (quot e 2) ;
  ;;       (if (odd? e) (mod (* b r) m) r))
  ;;     r))
)

(defn floor-sqrt
  [n]
  (if ({0 1} n)
    n
    (loop [lo 1
           hi n]
      (let [md (quot (+ lo hi) 2)
            md-sq (* md md)]
        (cond (= md-sq n) md
              (>= lo hi) (dec md)
              (< md-sq n) (recur (inc md) hi)
              :else (recur lo md))))))

(defn mod-log
  ;; https://cp-algorithms.com/algebra/discrete-log.html
  [b p m]
  (let [m-rt (inc (floor-sqrt m))
        n m-rt
        bn (loop [i 0
                  bn 1]
             (if (= i n) ;
               bn
               (recur (inc i) (mod (* b bn) m))))
        vs (loop [i 0
                  vs {}
                  v p]
             (if (= i (inc n)) ;
               vs
               (recur (inc i) (assoc vs v i) (mod (* v b) m))))]
    (loop [i 1
           v bn]
      (let [ani (get vs v 0)
            e (- (* i n) ani)]
        (cond ;
          (= i (inc n)) -1
          (> ani 0) e
          :else (recur (inc i) (mod (* v bn) m)))))))

(defn -main
  [& args]
  (let [prime 20201227
        [card-pubkey door-pubkey] (->> *in*
                                       (slurp)
                                       (clojure.string/split-lines)
                                       (map #(Integer/parseInt %))
                                       (vec))]
    ;; (clojure.pprint/pprint (map vector (range) (map floor-sqrt (range 100))))
    ;; part 1
    (let [card-loopsize (mod-log 7 card-pubkey prime)
          door-loopsize (mod-log 7 door-pubkey prime)
          card-enckey (mod-pow door-pubkey card-loopsize prime)
          door-enckey (mod-pow card-pubkey door-loopsize prime)]
      (assert (= card-enckey door-enckey))
      (println "part 1" card-enckey))))

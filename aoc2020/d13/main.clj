;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 13: Shuttle Search
;; url:       https://adventofcode.com/2020/day/13
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-08
;; execution: $ bash ./aoc2020/run.sh d13 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d13 d13/ex01.txt
;;            part 1 295
;;            part 2 1068781
;;            $ bash ./aoc2020/run.sh d13
;;            part 1 4722
;;            part 2 825305207525452
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d13.main
  (:require [clojure.math.numeric-tower]))

(defn parse-input
  [input-raw]
  (let [[_ depart-time-raw buses-id-raw] (re-matches #"^(\d+)\n(.*?)\n$"
                                                     input-raw)
        depart-time (Integer/parseInt depart-time-raw)
        buses (->> (clojure.string/split buses-id-raw #"\,")
                   (map vector (repeat :key) (range) (repeat :id))
                   (map #(apply hash-map %))
                   (filter #(not= (:id %) "x"))
                   (map (fn [bus] (update bus :id #(Integer/parseInt %))))
                   (map (fn [bus]
                          (assoc bus
                            :waiting-time (- (:id bus)
                                             (mod depart-time (:id bus))))))
                   (vec))]
    [depart-time buses]))

(defn extended-gcd
  "https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm"
  [a b]
  (cond (zero? a) [(clojure.math.numeric-tower/abs b) 0 1]
        (zero? b) [(clojure.math.numeric-tower/abs a) 1 0]
        :else
          (loop [s 0
                 s0 1
                 t 1
                 t0 0
                 r (clojure.math.numeric-tower/abs b)
                 r0 (clojure.math.numeric-tower/abs a)]
            (if (zero? r)
              [r0 s0 t0]
              (let [q (quot r0 r)]
                (recur (- s0 (* q s)) s (- t0 (* q t)) t (- r0 (* q r)) r))))))

(defn mod-mul-inv
  "https://rosettacode.org/wiki/Modular_inverse#Clojure"
  [a m]
  (let [m (if (neg? m) (- m) m)
        a (if (neg? a) (- m (mod (- a) m)) a)
        egcd (extended-gcd a m)]
    (assert (= (first egcd) 1) (str "No inverse since gcd is: " (first egcd)))
    (mod (second egcd) m)))

(defn congruence-system-solver
  "https://brilliant.org/wiki/chinese-remainder-theorem/"
  [a n]
  (let [m (apply * n)
        y (for [ni n] (/ m ni))
        z (for [[yi ni] (map vector y n)] (mod-mul-inv yi ni))
        x (apply + (for [[ai yi zi] (map vector a y z)] (* ai yi zi)))]
    (mod x m)))

(defn -main
  [& args]
  (let [[depart-time buses] (->> (first args)
                                 (slurp)
                                 (parse-input))]
    ;; (clojure.pprint/pprint buses)
    ;; part 1
    (let [bus-min-waiting-time (apply min-key :waiting-time buses)]
      (println "part 1"
               (* (:id bus-min-waiting-time)
                  (:waiting-time bus-min-waiting-time))))
    ;; part 2
    (let [min-depart-time-solution
            (congruence-system-solver (map - (map :key buses)) (map :id buses))]
      (println "part 2" min-depart-time-solution))))

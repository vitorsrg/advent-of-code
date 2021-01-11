;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 4: Passport Processing
;; url:       https://adventofcode.com/2020/day/4
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-02
;; execution: $ bash ./aoc2020/run.sh d04 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d04 < ./aoc2020/d04/input.txt
;;            part 1 206
;;            part 2 123
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d04.main
  (:require [clojure.string])
  (:require [clojure.walk]))

(defn parse-data
  [input-data]
  (->> (clojure.string/split input-data #"\n\n")
       (map (fn [block]
              (->> block
                   (re-seq #"(\S+):(\S+)")
                   (map rest)
                   (map vec))))
       (map #(into {} %))
       (map clojure.walk/keywordize-keys)))

(def p1-row-required-keys [:byr :iyr :eyr :hgt :hcl :ecl :pid])

(defn p1-row-is-valid? [row] (every? #(contains? row %) p1-row-required-keys))

(defn p2-row-is-valid?
  [row]
  (when (p1-row-is-valid? row)
    (let [{byr :byr, iyr :iyr, eyr :eyr, hgt :hgt, hcl :hcl, ecl :ecl, pid :pid}
            row]
      (and (and (= (count byr) 4) (<= 1920 (Integer/parseInt byr) 2002))
           (and (= (count iyr) 4) (<= 2010 (Integer/parseInt iyr) 2020))
           (and (= (count eyr) 4) (<= 2020 (Integer/parseInt eyr) 2030))
           (let [[number unit] (rest (re-find #"(\d+)(\w+)" hgt))
                 value (Integer/parseInt number)]
             (case unit
               "cm" (and (<= 150 value 193))
               "in" (and (<= 59 value 76))
               false))
           (some? (re-matches #"\#[0-9a-f]{6}" hcl))
           (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
           (some? (re-matches #"\d{9}" pid))))))

(defn -main
  [& args]
  (let [input-file-name *in*
        input-rows (parse-data (slurp input-file-name))]
    ;; part 1
    (let [input-row-is-valid-count (count (filter p1-row-is-valid? input-rows))]
      (println "part 1" input-row-is-valid-count))
    ;; part 2
    (let [input-row-is-valid-count (count (filter p2-row-is-valid? input-rows))]
      (println "part 2" input-row-is-valid-count))))

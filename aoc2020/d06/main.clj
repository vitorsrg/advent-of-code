;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 6: Custom Customs:
;; url:       https://adventofcode.com/2020/day/6
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-03
;; execution: $ bash ./aoc2020/run.sh d06
;; example:
;;            $ bash ./aoc2020/run.sh d06
;;            part 1 6714
;;            part 2 3435
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d06.main)

(defn parse-group
  [group-data]
  (let [person-count (->> group-data
                          (re-seq #"\n[^$]")
                          (count)
                          (+ 1))
        yes-answers (re-seq #"\w" group-data)
        yes-answers-any (set yes-answers)
        yes-answers-any-count (count yes-answers-any)
        yes-answers-freq (frequencies yes-answers)
        yes-answers-all (->> yes-answers-freq
                             (filter #(= (second %) person-count))
                             (map second))
        yes-answers-all-count (count yes-answers-all)]
    {:group-data group-data,
     :person-count person-count,
     :yes-answers yes-answers,
     :yes-answers-any yes-answers-any,
     :yes-answers-any-count yes-answers-any-count,
     :yes-answers-freq yes-answers-freq,
     :yes-answers-all yes-answers-all,
     :yes-answers-all-count yes-answers-all-count}))

(defn -main
  [& args]
  (let [groups (->> (first args)
                    (slurp)
                    (#(clojure.string/split % #"\n\n"))
                    (map parse-group)
                    (vec))]
    ;; (run! println groups)
    ;; part 1
    (let [yes-answers-any-count-sum (->> groups
                                         (map :yes-answers-any-count)
                                         (reduce +))]
      (println "part 1" yes-answers-any-count-sum))
    ;; part 2
    (let [yes-answers-all-count-sum (->> groups
                                         (map :yes-answers-all-count)
                                         (reduce +))]
      (println "part 2" yes-answers-all-count-sum))))

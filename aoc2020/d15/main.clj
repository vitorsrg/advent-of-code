;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 15: Rambunctious Recitation
;; url:       https://adventofcode.com/2020/day/15
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-09
;; execution: $ bash ./aoc2020/run.sh d15
;; example:
;;            $ bash ./aoc2020/run.sh d15
;;            part 1 1294
;;            part 2 573522
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d15.main)

(defn sequence-simulate
  [transition initial-state]
  (reductions (fn [state _] (transition state)) initial-state (range)))

(defn sequence-transition
  [last-state]
  (let [{last-index :index,
         [seed-element & next-seed] :seed,
         last-last-element-last-index :last-element-last-index,
         last-element :element}
          last-state]
    (cond ;
      (nil? last-index) ;
        {:index 1,
         :seed next-seed,
         :last-element-last-index {},
         :element seed-element}
      (some? seed-element) ;
        {:index (inc last-index),
         :seed next-seed,
         :last-element-last-index (assoc last-last-element-last-index
                                    last-element last-index),
         :element seed-element}
      :else ;
        (let [element (if (contains? last-last-element-last-index last-element)
                        (- last-index
                           (get last-last-element-last-index last-element))
                        0)]
          {:index (inc last-index),
           :seed nil,
           :last-element-last-index (assoc last-last-element-last-index
                                      last-element last-index),
           :element element}))))

(defn -main
  [& args]
  ;; (clojure.pprint/pprint (->> {:seed [0 3 6]}
  ;;                             (sequence-simulate sequence-transition)
  ;;                             (rest)
  ;;                             (take 10)
  ;;                             (map :element)
  ;;                             (vec)))
  (println "part 1"
           (->> {:seed [1 0 16 5 17 4]}
                (sequence-simulate sequence-transition)
                (rest)
                (take 2020)
                (map :element)
                (last)))
  (println "part 2"
           (->> {:seed [1 0 16 5 17 4]}
                (sequence-simulate sequence-transition)
                (rest)
                (take 30000000)
                (map :element)
                (last))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 12: Rain Risk
;; url:       https://adventofcode.com/2020/day/12
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-08
;; execution: $ bash ./aoc2020/run.sh d12 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d12 d12/ex01.txt
;;            part 1 25
;;            part 2 286
;;            $ bash ./aoc2020/run.sh d12
;;            part 1 582
;;            part 2 52069
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d12.main)

(defn parse-action
  [action-raw]
  (let [[_ func arg] (re-matches #"^([a-zA-Z]+)(\d+)$" action-raw)]
    [(keyword func) (Integer/parseInt arg)]))

(defn p1-ship-next-state
  [state action]
  (let [[x y d] state
        [f v] action]
    (case f
      :N [x (+ y v) d]
      :S [x (- y v) d]
      :E [(+ x v) y d]
      :W [(- x v) y d]
      :L [x y (mod (+ d v) 360)]
      :R [x y (mod (- d v) 360)]
      :F (case d
           0 [(+ x v) y d]
           90 [x (+ y v) d]
           180 [(- x v) y d]
           270 [x (- y v) d]))))

(defn rotate-vector
  [vector degrees]
  (assert (= (mod degrees 90) 0))
  (if (< degrees 0)
    (rotate-vector vector (mod degrees 360))
    (let [[vx vy] vector
          [vrx vry] (case degrees
                      0 [vx vy]
                      90 [(- vy) vx]
                      180 [(- vx) (- vy)]
                      270 [vy (- vx)])]
      [vrx vry])))

(defn p2-ship-next-state
  [state action]
  (let [[sx sy wx wy] state
        [f v] action]
    (case f
      :N [sx sy wx (+ wy v)]
      :S [sx sy wx (- wy v)]
      :E [sx sy (+ wx v) wy]
      :W [sx sy (- wx v) wy]
      :L (concat [sx sy] (rotate-vector [wx wy] v))
      :R (concat [sx sy] (rotate-vector [wx wy] (- v)))
      :F [(+ sx (* v wx)) (+ sy (* v wy)) wx wy])))

(defn ship-simulate
  [ship-transition ship-initial-state actions]
  (reductions ship-transition ship-initial-state actions))

(defn -main
  [& args]
  (let [actions (->> (first args)
                     (slurp)
                     (clojure.string/split-lines)
                     (map parse-action)
                     (map vec)
                     (vec))]
    ;; (clojure.pprint/pprint actions)
    ;; part 1
    (let [ship-states (vec (ship-simulate p1-ship-next-state [0 0 0] actions))
          [x y _] (nth ship-states (count actions))]
      ;; (clojure.pprint/pprint ship-states)
      (println "part 1" (+ (Math/abs x) (Math/abs y))))
    ;; part 2
    (let [ship-states (vec
                        (ship-simulate p2-ship-next-state [0 0 10 1] actions))
          [x y _] (nth ship-states (count actions))]
      ;; (clojure.pprint/pprint ship-states)
      (println "part 2" (+ (Math/abs x) (Math/abs y))))))

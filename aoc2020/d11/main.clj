;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 11: Seating System
;; url:       https://adventofcode.com/2020/day/11
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-07
;; execution: $ bash ./aoc2020/run.sh d11 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d11 d11/ex01.txt
;;            part 1 37
;;            part 2 26
;;            $ bash ./aoc2020/run.sh d11
;;            part 1
;;            part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d11.main)

(defn p1-get-seat-adjacency-occupancy
  [seat-grid i j]
  (let [h (count seat-grid)
        w (count (nth seat-grid 0))]
    (->> [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]]
         (map (fn [[di dj]] [(+ i di) (+ j dj)]))
         (filter (fn [[ni nj]] (and (< -1 ni h) (< -1 nj w))))
         (map (fn [[ni nj]] (get-in seat-grid [ni nj])))
         (filter #(= \# %))
         (count))))

(defn p1-seat-next-state
  [seat-grid i j]
  (let [state (get-in seat-grid [i j])
        adjacency-occupancy (p1-get-seat-adjacency-occupancy seat-grid i j)]
    (cond (and (= state \L) (= adjacency-occupancy 0)) \#
          (and (= state \#) (>= adjacency-occupancy 4)) \L
          :else state)))

(defn p2-get-seat-adjacency-occupancy
  [seat-grid i j]
  (let [h (count seat-grid)
        w (count (nth seat-grid 0))]
    (->> [[-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1]]
         (map (fn [[di dj]]
                (->> (range)
                     (map inc)
                     (map (fn [step] [(+ i (* di step)) (+ j (* dj step))]))
                     (take-while (fn [[ni nj]] (and (< -1 ni h) (< -1 nj w))))
                     (map (fn [[ni nj]] (get-in seat-grid [ni nj])))
                     (filter #(contains? #{\L \#} %)))))
         (map first)
         (filter #(= \# %))
         (filter some?)
         (count))))

(defn p2-seat-next-state
  [seat-grid i j]
  (let [state (get-in seat-grid [i j])
        adjacency-occupancy (p2-get-seat-adjacency-occupancy seat-grid i j)]
    (cond (and (= state \L) (= adjacency-occupancy 0)) \#
          (and (= state \#) (>= adjacency-occupancy 5)) \L
          :else state)))

(defn seat-grid-next-state
  [seat-grid seat-next-state-fn]
  (let [h (count seat-grid)
        w (count (nth seat-grid 0))]
    (->> (for [i (range h)]
           (for [j (range w)] (seat-next-state-fn seat-grid i j)))
         (map vec)
         (vec))))

(defn seat-grid-simulate-states
  [seat-grid seat-next-state-fn]
  (loop [seat-grid-state seat-grid
         states [seat-grid-state]]
    (let [next-seat-grid-state (seat-grid-next-state seat-grid-state
                                                     seat-next-state-fn)]
      ;; (clojure.pprint/pprint next-seat-grid-state)
      (if (= seat-grid-state next-seat-grid-state)
        states
        (recur next-seat-grid-state (conj states next-seat-grid-state))))))

(defn -main
  [& args]
  (let [seat-grid (->> (first args)
                       (slurp)
                       (clojure.string/split-lines)
                       (map vec)
                       (vec))]
    ;; (clojure.pprint/pprint seat-grid)
    ;; (println (p2-get-seat-adjacency-occupancy seat-grid 0 3))
    ;; part 1
    (let [seat-grid-states (seat-grid-simulate-states seat-grid
                                                      p1-seat-next-state)
          seat-grid-last-state (->> seat-grid-states
                                    (count)
                                    (dec)
                                    (nth seat-grid-states))
          seat-grid-occupied-count (->> seat-grid-last-state
                                        (apply concat)
                                        (filter #(= \# %))
                                        (count))]
      (println "part 1" seat-grid-occupied-count))
    ;; part 2
    (let [seat-grid-states (seat-grid-simulate-states seat-grid
                                                      p2-seat-next-state)
          seat-grid-last-state (->> seat-grid-states
                                    (count)
                                    (dec)
                                    (nth seat-grid-states))
          seat-grid-occupied-count (->> seat-grid-last-state
                                        (apply concat)
                                        (filter #(= \# %))
                                        (count))]
      ;; (run! println
      ;;       (->> seat-grid-states
      ;;            (map (fn [seat-grid-state]
      ;;                   (->> seat-grid-state
      ;;                        (map #(apply str %))
      ;;                        (clojure.string/join "\n"))))
      ;;            (map #(str % "\n\n"))))
      (println "part 2" seat-grid-occupied-count))))

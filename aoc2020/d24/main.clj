;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 24: Lobby Layout
;; url:       https://adventofcode.com/2020/day/24
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-02-01
;; execution: $ bash ./aoc2020/run.sh d24 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d24 < ./aoc2020/d24/ex01.txt
;;            part 1 10
;;            part 2 2208
;;            $ bash ./aoc2020/run.sh d24 < ./aoc2020/d24/input.txt
;;            part 1 277
;;            part 2 3531
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d24.main)

(defn transpose [coll] (apply mapv vector coll))

(defn parse-path
  [path-raw]
  (->> path-raw
       (re-seq #"e|se|sw|w|nw|ne")
       (map keyword)
       (vec)))

(def direction-to-step
  ;; https://www.redblobgames.com/grids/hexagons/#coordinates-cube
  {:e [1 -1 0], ;
   :se [0 -1 1], ;
   :sw [-1 0 1], ;
   :w [-1 1 0], ;
   :nw [0 1 -1], ;
   :ne [1 0 -1] ;
  })

(defn path-to-tile
  [path]
  (->> path
       (map direction-to-step)
       (transpose)
       (map #(apply + %))))

(defn tile-neighbourhood
  [tile]
  (->> direction-to-step
       (map second)
       (map (fn [step]
              (->> [tile step]
                   (transpose)
                   (map #(apply + %)))))))

(defn tile-transition
  [_ black-tiles tile state]
  (let [neighbourhood (tile-neighbourhood tile)
        neighbourhood-black-count (->> neighbourhood
                                       (filter #(contains? black-tiles %))
                                       (count))]
    (case state ;
      :black ;
        (if (or (= neighbourhood-black-count 0) (> neighbourhood-black-count 2))
          :white
          :black)
      :white ;
        (if (= neighbourhood-black-count 2) :black :white))))

(defn sequence--simulate
  [sequence transition initial-state]
  (reductions (fn [state _] (transition sequence state)) initial-state (range)))

(defn sequence--transition
  [sequence state]
  (let [{black-tiles :black-tiles} state
        applicable-tiles
          (->> black-tiles
               (map (fn [tile] (cons tile (tile-neighbourhood tile))))
               (apply concat)
               (set)
               (map (fn [tile]
                      [tile (if (contains? black-tiles tile) :black :white)]))
               (into {}))
        next-black-tiles
          (->> applicable-tiles
               (map (fn [[tile state]]
                      [tile (tile-transition sequence black-tiles tile state)]))
               (filter #(= (second %) :black))
               (map first)
               (set))]
    {:black-tiles next-black-tiles}))

(defn -main
  [& args]
  (let [paths (->> *in*
                   (slurp)
                   (clojure.string/split-lines)
                   (map parse-path)
                   (vec))
        black-tiles (->> paths
                         (map path-to-tile)
                         (frequencies)
                         (filter #(odd? (second %)))
                         (map first)
                         (set))]
    ;; (clojure.pprint/pprint paths)
    ;; part 1
    (println "part 1" (count black-tiles))
    ;; part 2
    (println "part 2"
             (->> {:black-tiles black-tiles}
                  (sequence--simulate {} sequence--transition)
                  (rest)
                  (take 100)
                  (map :black-tiles)
                  (map count)
                  (vec)
                  (last)))))

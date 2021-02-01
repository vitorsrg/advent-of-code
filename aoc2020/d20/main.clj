;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 20: Jurassic Jigsaw
;; url:       https://adventofcode.com/2020/day/20
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-14
;; execution: $ bash ./aoc2020/run.sh d20 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d20 < ./aoc2020/d20/ex01.txt
;;            part 1 20899048083289
;;            part 2 273
;;            $ bash ./aoc2020/run.sh d20
;;            part 1 8272903687921
;;            part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d20.main
  (:require [clojure.math.numeric-tower]))

(defn nthv
  [v i]
  (let [n (count v)]
    (cond (<= 0 i (dec n)) (nth v i)
          (<= 0 (+ n i) (dec n)) (nth v (+ n i))
          :else (nth v i))))

(defn parse-tile
  [tile-raw]
  (let [[_ key-raw data-raw]
          (re-matches #"(?s)^[\s\n]*Tile (\d+):\n(.*?)[\s\n]*$" tile-raw)
        data (->> data-raw
                  (clojure.string/split-lines)
                  (map vec)
                  (vec))]
    {:key (Integer/parseInt key-raw),
     :data data,
     :borders {;; [deg flipped] -> border
               ;; :top-front
               [0 false] (-> data
                             (nthv 0)
                             (vec)),
               ;; :right-front
               [90 false] (->> data
                               (map #(nthv % -1))
                               (vec)),
               ;; :bottom-front
               [180 false] (-> data
                               (nthv -1)
                               (reverse)
                               (vec)),
               ;; :left-front
               [270 false] (->> data
                                (map #(nthv % 0))
                                (reverse)
                                (vec)),
               ;; :top-back
               [0 true] (-> data
                            (nthv 0)
                            (reverse)
                            (vec)),
               ;; :right-back
               [90 true] (->> data
                              (map #(nthv % -1))
                              (reverse)
                              (vec)),
               ;; :bottom-back
               [180 true] (-> data
                              (nthv -1)
                              (vec)),
               ;; :left-back
               [270 true] (->> data
                               (map #(nthv % 0))
                               (vec))}}))

(defn parse-tiles-doc
  [tiles-doc-raw]
  (let [tiles-raw (clojure.string/split tiles-doc-raw #"\n\n")
        tiles-list (->> tiles-raw
                        (map parse-tile)
                        (vec))
        tiles-keys (->> tiles-list
                        (map :key)
                        (vec))
        tiles (->> tiles-list
                   (map (fn [tile] [(:key tile) tile]))
                   (into {}))]
    {:tiles-keys tiles-keys, :tiles tiles}))

(defn problem--create
  [tiles border-to-tiles]
  {:tiles tiles,
   :border-to-tiles border-to-tiles,
   :tile-count (count tiles),
   :board-width (clojure.math.numeric-tower/sqrt (count tiles)),
   :border-size (->> tiles
                     (first)
                     (second)
                     (:data)
                     (count))})

(defn problem--state-actions
  [problem state]
  (let [index (:index state)
        board-width (:board-width problem)
        available-tiles (:available-tiles state)
        tile-behind-index (if (< 0 (mod index board-width)) (dec index))
        tile-behind-right-border
          (if (some? tile-behind-index)
            (let [[tile-behind-key [tile-behind-deg tile-behind-flipped]]
                    (nth (:board state) tile-behind-index)]
              (->> tile-behind-key
                   (get (:tiles problem))
                   (:borders)
                   (#(get %
                          [(if tile-behind-flipped
                             (mod (- tile-behind-deg 90) 360)
                             (mod (+ tile-behind-deg 90) 360))
                           tile-behind-flipped])))))
        tile-left-border (->> tile-behind-right-border
                              (reverse)
                              (vec))
        tile-candidates-by-left-border
          (some->> tile-left-border
                   (get (:border-to-tiles problem))
                   (filter (fn [[key _]] (contains? available-tiles key)))
                   (map (fn [[key [deg flipped]]] [key
                                                   [(if flipped
                                                      (mod (+ deg 270) 360)
                                                      (mod (- deg 270) 360))
                                                    flipped]]))
                   (set))
        tile-above-index (if (>= index board-width) (- index board-width))
        tile-above-bottom-border
          (if (some? tile-above-index)
            (let [[tile-above-key [tile-above-deg tile-above-flipped]]
                    (nth (:board state) tile-above-index)]
              (->> tile-above-key
                   (get (:tiles problem))
                   (:borders)
                   (#(get %
                          [(if tile-above-flipped
                             (mod (- tile-above-deg 180) 360)
                             (mod (+ tile-above-deg 180) 360))
                           tile-above-flipped])))))
        tile-top-border (->> tile-above-bottom-border
                             (reverse)
                             (vec))
        tile-candidates-by-top-border
          (some->> tile-top-border
                   (get (:border-to-tiles problem))
                   (filter (fn [[key _]] (contains? available-tiles key)))
                   (set))]
    (cond ;
      (and (some? tile-candidates-by-left-border)
           (some? tile-candidates-by-top-border)) ;
        (clojure.set/intersection tile-candidates-by-left-border
                                  tile-candidates-by-top-border)
      (and (some? tile-candidates-by-left-border)) ;
        tile-candidates-by-left-border
      (and (some? tile-candidates-by-top-border)) ;
        tile-candidates-by-top-border
      :else ;
        (->> problem
             (:border-to-tiles)
             (map second)
             (apply clojure.set/union)))))

(defn problem--make-initial-state
  [problem]
  {:index 0,
   :available-tiles (->> problem
                         (:tiles)
                         (map first)
                         (set)),
   :board []})

(defn problem--is-final-state?
  [problem state]
  (= (count (:board state)) (:tile-count problem)))

(defn problem--next-state
  [problem state action]
  (let [{index :index,
         available-tiles :available-tiles,
         board :board,
         used-tiles :used-tiles}
          state
        [tile-key tile-state] action]
    {:index (inc index),
     :available-tiles (disj available-tiles tile-key),
     :board (conj board action)}))

(defn problem--simulate-greedy
  [problem state]
  (if (problem--is-final-state? problem state)
    [[state nil]]
    (first (for [action (problem--state-actions problem state)
                 :let [next-state (problem--next-state problem state action)
                       result (problem--simulate-greedy problem next-state)]
                 :when (some? result)]
             (lazy-seq (cons [state action] result))))))

(defn tile-apply-state
  [data [deg flipped]]
  (cond ;
    (>= deg 90) (tile-apply-state (->> data
                                       (map reverse)
                                       (apply map vector)
                                       (vec))
                                  [(- deg 90) flipped])
    (true? flipped) (->> data
                         (map reverse)
                         (map vec)
                         (vec))
    :else data))

(defn -main
  [& args]
  (let [tiles-doc (->> *in*
                       (slurp)
                       (parse-tiles-doc))
        border-to-tiles
          (->> tiles-doc
               (:tiles)
               (map (fn [[key tile]]
                      (->> tile
                           :borders
                           (map (fn [[type border]] [border [key type]])))))
               (apply concat)
               (reduce (fn [m [k v]] (assoc m k (conj (get m k #{}) v))) {}))
        problem (problem--create (:tiles tiles-doc) border-to-tiles)
        board-width (:board-width problem)
        border-size (:border-size problem)
        initial-state (problem--make-initial-state problem)
        ;; initial-state
        ;;   (problem--next-state problem initial-state [1951 [180 true]])
        problem-solution (->> initial-state
                              (problem--simulate-greedy problem)
                              (vec))
        solution-board (->> problem-solution
                            (#(nthv % -1))
                            (first)
                            (:board)
                            ;; (map first)
                            (partition board-width)
                            (map vec)
                            (vec))]
    ;; (clojure.pprint/pprint tiles-doc)
    ;; (clojure.pprint/pprint solution-board)
    (assert (some->> tiles-doc
                     (:tiles)
                     (map second)
                     (map :borders)
                     (map (fn [borders]
                            (->> borders
                                 (map (fn [[[deg flipped] border]]
                                        (= border
                                           (->> [deg (not flipped)]
                                                (get borders)
                                                (reverse)
                                                (vec)))))
                                 (every? identity))))
                     (every? identity)))
    ;; part 1
    (let [corner-key-mul (* (get-in solution-board [0 0 0])
                            (get-in solution-board [0 (dec board-width) 0])
                            (get-in solution-board [(dec board-width) 0 0])
                            (get-in solution-board
                                    [(dec board-width) (dec board-width) 0]))]
      (println "part 1" corner-key-mul))
    ;; part 2
    (let [solution-image-dict
            (->>
              (for [board-row (range board-width)
                    board-col (range board-width)
                    :let [[tile-key tile-status] (get-in solution-board
                                                         [board-row board-col])
                          tile-data (->> tile-key
                                         (get (:tiles tiles-doc))
                                         (:data)
                                         (#(tile-apply-state % tile-status)))]]
                (for [tile-row (range 1 (dec border-size))
                      tile-col (range 1 (dec border-size))
                      :let [value (get-in tile-data [tile-row tile-col])
                            image-row (+ (* board-row (- border-size 2))
                                         (- tile-row 1))
                            image-col (+ (* board-col (- border-size 2))
                                         (- tile-col 1))]]
                  [[image-row image-col] value]))
              (apply concat)
              (into {}))
          solution-image-width (* board-width (- border-size 2))
          solution-image-arr (vec
                               (for [row (range solution-image-width)]
                                 (vec (for [col (range solution-image-width)]
                                        (get solution-image-dict [row col])))))
          solution-image-arrs (->> [[0 false] [90 false] [180 false] [270 false]
                                    [0 true] [90 true] [180 true] [270 true]]
                                   (mapv #(tile-apply-state solution-image-arr
                                                            %)))
          solution-image-strs (mapv (fn [solution-image-arr]
                                      (->> solution-image-arr
                                           (map #(apply str %))
                                           (clojure.string/join "\n")))
                                solution-image-arrs)
          monster-strs ["                  # " ;
                        "#    ##    ##    ###" ;
                        " #  #  #  #  #  #   "]
          monster-regex
            (->> monster-strs
                 (clojure.string/join
                   (str "[\\.#\\n]{"
                        (inc (- (* board-width (- border-size 2))
                                (count (nth monster-strs 0))))
                        "}"))
                 (#(clojure.string/replace %
                                           #"\s"
                                           (clojure.string/re-quote-replacement
                                             "[\\.#]")))
                 (#(apply str "(?s)^" % ".*")))
          solution-image-monsters
            (->> solution-image-strs
                 ;; because the regex overlap zzz
                 (map (fn [solution-image-str]
                        (->> solution-image-str
                             (reductions (fn [s _] (rest s)) solution-image-str)
                             (map #(apply str %))
                             (map #(re-matches (re-pattern monster-regex) %))
                             (filter some?)
                             (count))))
                 ;; (map #(re-seq (re-pattern monster-regex) %))
                 ;; (map count)
                 (map vector (range))
                 (filter #(> (second %) 0))
                 (vec))
          monsters-count (->> solution-image-monsters
                              (first)
                              (second))]
      ;; (println (->> solution-image-monsters
      ;;               (first)
      ;;               (first)
      ;;               (nth solution-image-strs)))
      (assert (= (count solution-image-monsters) 1))
      (println "part 2"
               (- (->> 0
                       (nth solution-image-strs)
                       (filter #(= % \#))
                       (count))
                  (->> monster-strs
                       (clojure.string/join "")
                       (filter #(= % \#))
                       (count)
                       (* monsters-count)))))))

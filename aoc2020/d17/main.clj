;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 17: Conway Cubes
;; url:       https://adventofcode.com/2020/day/17
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-10
;; execution: $ bash ./aoc2020/run.sh d17 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d17 < ./aoc2020/d17/ex01.txt
;;            part 1 112
;;            part 2 848
;;            $ bash ./aoc2020/run.sh d17 < ./aoc2020/d17/input.txt
;;            part 1 293
;;            part 2 1816
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d17.main)

(defn product-colls
  [colls]
  (if (empty? colls)
    '(())
    (for [more (product-colls (rest colls)) x (first colls)] (cons x more))))

(defn p1-cell-neighbourhood
  [[x y z]]
  (->> [-1 0 1]
       (repeat 3)
       (product-colls)
       (filter (fn [[dx dy dz]] (or (not= dx 0) (not= dy 0) (not= dz 0))))
       (map (fn [[dx dy dz]] [(+ x dx) (+ y dy) (+ z dz)]))))

(defn p2-cell-neighbourhood
  [[x y z w]]
  (->> [-1 0 1]
       (repeat 4)
       (product-colls)
       (filter (fn [[dx dy dz dw]]
                 (or (not= dx 0) (not= dy 0) (not= dz 0) (not= dw 0))))
       (map (fn [[dx dy dz dw]] [(+ x dx) (+ y dy) (+ z dz) (+ w dw)]))))

(defn cell-transition
  [sequence alive-cells loc state]
  (let [{cell-neighbourhood-fn :cell-neighbourhood-fn} sequence
        neighbourhood (cell-neighbourhood-fn loc)
        neighbourhood-active-count (->> neighbourhood
                                        (filter #(contains? alive-cells %))
                                        (count))]
    (case state ;
      :active
        (if (contains? #{2 3} neighbourhood-active-count) :active :inactive)
      :inactive ;
        (if (= neighbourhood-active-count 3) :active :inactive))))

(defn sequence--simulate
  [sequence transition initial-state]
  (reductions (fn [state _] (transition sequence state)) initial-state (range)))

(defn sequence--transition
  [sequence state]
  (let [{cell-neighbourhood-fn :cell-neighbourhood-fn} sequence
        {alive-cells :alive-cells} state
        applicable-cells
          (->>
            alive-cells
            (map (fn [cell] (cons cell (cell-neighbourhood-fn cell))))
            (reduce (fn [cell-set cell-group] (apply conj cell-set cell-group)))
            (map (fn [cell]
                   [cell (if (contains? alive-cells cell) :active :inactive)]))
            (into {}))
        next-alive-cells
          (->> applicable-cells
               (map (fn [[loc state]]
                      [loc (cell-transition sequence alive-cells loc state)]))
               (filter #(= (second %) :active))
               (map first)
               (set))]
    {:alive-cells next-alive-cells}))

(defn -main
  [& args]
  (let [slice-input (->> *in*
                         (slurp)
                         (clojure.string/split-lines)
                         (map vec)
                         (vec))]
    ;; (println (->> slice-input
    ;;               (map #(apply str %))
    ;;               (clojure.string/join "\n")))
    ;; part 1
    (let [p1-alive-cells (->> (for [x (range (count slice-input))
                                    y (range (count (nth slice-input 0)))
                                    :let [value (get-in slice-input [x y])]
                                    :when (= value \#)]
                                [x y 0])
                              (set))
          p1-sequence {:cell-neighbourhood-fn p1-cell-neighbourhood}
          p1-initial-state {:alive-cells p1-alive-cells}]
      (println "part 1"
               (->> p1-initial-state
                    (sequence--simulate p1-sequence sequence--transition)
                    (rest)
                    (take 6)
                    (map :alive-cells)
                    (map count)
                    (vec)
                    (last))))
    ;; part 2
    (let [p2-alive-cells (->> (for [x (range (count slice-input))
                                    y (range (count (nth slice-input 0)))
                                    :let [value (get-in slice-input [x y])]
                                    :when (= value \#)]
                                [x y 0 0])
                              (set))
          p2-sequence {:cell-neighbourhood-fn p2-cell-neighbourhood}
          p2-initial-state {:alive-cells p2-alive-cells}]
      (println "part 2"
               (->> p2-initial-state
                    (sequence--simulate p2-sequence sequence--transition)
                    (rest)
                    (take 6)
                    (map :alive-cells)
                    (map count)
                    (vec)
                    (last))))))

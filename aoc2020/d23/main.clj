;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 23: Crab Cups
;; url:       https://adventofcode.com/2020/day/23
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-17
;; execution: $ bash ./aoc2020/run.sh d23 input
;; example:
;;            $ bash ./aoc2020/run.sh d23 389125467
;;            part 1 67384529
;;            part 2 149245887792
;;            $ bash ./aoc2020/run.sh d23 963275481
;;            part 1 97632548
;;            part 2 412990492266
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d23.main)

(defn problem--recur
  [problem transition initial-state]
  (let [next-state (transition problem initial-state)]
    (if (some? next-state)
      (lazy-seq (cons next-state
                      (problem--recur problem transition next-state)))
      [])))

(defn p1--problem--next-state
  [problem state]
  ;; (println problem state)
  (let [{max-rounds :max-rounds} problem
        {round :round, cups-state :cups-state} state
        current-cup (first cups-state)
        picked-cups (take 3 (drop 1 cups-state))
        cups-tail (drop 4 cups-state)
        dest-cup (or (some->> cups-tail
                              (filter #(< % current-cup))
                              (seq)
                              (apply max))
                     (when cups-tail (apply max cups-tail)))
        before-dest-cup (take-while #(not= % dest-cup) cups-tail)
        after-dest-cup (rest (drop-while #(not= % dest-cup) cups-tail))]
    ;; (println round
    ;;          "\t|"
    ;;          current-cup
    ;;          picked-cups
    ;;          before-dest-cup
    ;;          dest-cup
    ;;          after-dest-cup)
    (if (<= round max-rounds)
      {:round (inc round),
       :cups-state (concat before-dest-cup
                           [dest-cup]
                           picked-cups
                           after-dest-cup
                           [current-cup])})))

(defn p2--problem--create
  [max-rounds initial-cups-state]
  (assert (= (set initial-cups-state)
             (->> initial-cups-state
                  (count)
                  (inc)
                  (range 1)
                  (set))))
  {:max-rounds max-rounds, :initial-cups-state initial-cups-state})

(defn p2--problem--make-initial-state
  [problem]
  (let [{initial-cups-state :initial-cups-state} problem
        cup-count (count initial-cups-state)]
    {:round 1,
     :current-cup (first initial-cups-state),
     :cup-next (->> (range cup-count)
                    (map (fn [i] [(nth initial-cups-state i)
                                  (nth initial-cups-state
                                       (mod (inc i) cup-count))]))
                    (into {}))}))

(defn p2--problem--next-state
  [problem state]
  ;; (println problem state)
  (let [{max-rounds :max-rounds} problem
        {round :round, current-cup :current-cup, cup-next :cup-next} state
        picked-cups (->> (range 3)
                         (reductions (fn [cup _] (get cup-next cup))
                                     current-cup)
                         (rest)
                         (vec))
        dest-cup-deny-set (conj (set picked-cups) current-cup)
        dest-cup (or (some->> (range current-cup 0 -1)
                              (filter #(not (contains? dest-cup-deny-set %)))
                              (first))
                     (some->> (range (count cup-next) 0 -1)
                              (filter #(not (contains? dest-cup-deny-set %)))
                              (first)))]
    ;; (println round
    ;;          "\t|"
    ;;          (->> (range)
    ;;               (reductions (fn [cup _] (get cup-next cup)) current-cup)
    ;;               (rest)
    ;;               (take-while #(not= % current-cup))
    ;;               (cons current-cup)
    ;;               (vec))
    ;;          "\t|"
    ;;          current-cup
    ;;          picked-cups
    ;;          dest-cup)
    (if (<= round max-rounds)
      {:round (inc round),
       :current-cup (get cup-next (nth picked-cups 2)),
       :cup-next (assoc cup-next
                   current-cup (get cup-next (nth picked-cups 2))
                   dest-cup (nth picked-cups 0)
                   (nth picked-cups 2) (get cup-next dest-cup))})))

(defn -main
  [& args]
  (let [initial-cups-state (->> args
                                (first)
                                (map str)
                                (map #(Integer/parseInt %))
                                (vec))]
    ;; (clojure.pprint/pprint initial-cups-state)
    (assert (some #(= 1 %) initial-cups-state))
    ;; part 1
    (let [problem {:max-rounds 100}
          initial-state {:round 1, :cups-state initial-cups-state}
          problem-simulation
            (problem--recur problem p1--problem--next-state initial-state)
          final-state (last problem-simulation)
          final-cups-state (:cups-state final-state)
          before-1-cup (take-while #(not= % 1) final-cups-state)
          after-1-cup (rest (drop-while #(not= % 1) final-cups-state))]
      (println "part 1" (apply str (concat after-1-cup before-1-cup))))
    ;; part 2
    (let [initial-cups-state-extended
            (->> (range (inc (count initial-cups-state)) (inc 1000000))
                 (concat initial-cups-state)
                 (vec))
          problem (p2--problem--create 10000000 initial-cups-state-extended)
          initial-state (p2--problem--make-initial-state problem)
          problem-simulation
            (problem--recur problem p2--problem--next-state initial-state)
          final-state (last problem-simulation)]
      (println
        "part 2"
        (->> (range 2)
             (reductions (fn [cup _] (get (:cup-next final-state) cup)) 1)
             (rest)
             (apply *)))
      ;
    )))

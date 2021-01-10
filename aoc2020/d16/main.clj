;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 16: Ticket Translation
;; url:       https://adventofcode.com/2020/day/16
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      yyyy-MM-dd
;; execution: $ bash ./aoc2020/run.sh d16 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d16
;;            part 1 23044
;;            part 2 3765150732757
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d16.main)

(defn transpose [coll] (apply mapv vector coll))

(defn product-colls
  [colls]
  "https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Clojure"
  (if (empty? colls)
    '(())
    (for [more (product-colls (rest colls)) x (first colls)] (cons x more))))

(defn parse-data
  [data-raw]
  (let [[_ fields-raw own-ticket-raw nearby-tickets-raw]
          (re-matches #"(?s)^(.*?)\n\n.*?\n(.*?)\n\n.*?\n(.*?)\n$" data-raw)
        fields (->> fields-raw
                    (clojure.string/split-lines)
                    (map #(re-matches #"^(.*?): (\d+)-(\d+) or (\d+)-(\d+)$" %))
                    (map rest)
                    (mapv (fn [[name & values]]
                            {:name name,
                             :constraint (mapv #(Integer/parseInt %) values)})))
        own-ticket (->> own-ticket-raw
                        (#(clojure.string/split % #","))
                        (mapv #(Integer/parseInt %)))
        nearby-tickets (->> nearby-tickets-raw
                            (clojure.string/split-lines)
                            (map #(clojure.string/split % #","))
                            (mapv (fn [ticket]
                                    (mapv #(Integer/parseInt %) ticket))))]
    {:fields fields, ;
     :own-ticket own-ticket,
     :nearby-tickets nearby-tickets}))

(defn field-validate
  [constraint value]
  (or (<= (nth constraint 0) value (nth constraint 1))
      (<= (nth constraint 2) value (nth constraint 3))))

(defn problem--create
  [fields cols-valid-fields]
  {:fields-names fields, :cols-valid-fields cols-valid-fields})

(defn problem--make-initial-state
  [problem]
  {:index 0,
   :available-fields (set (:fields-names problem)),
   :fields-order [],
   :used-fields #{}})

(defn problem--state-actions
  [problem state]
  (clojure.set/difference (nth (:cols-valid-fields problem) (:index state))
                          (:used-fields state)))

(defn problem--is-final-state?
  [problem state]
  (= (:index state) (count (:fields-names problem))))

(defn problem--next-state
  [problem state action]
  (let [{index :index,
         available-fields :available-fields,
         fields-order :fields-order,
         used-fields :used-fields}
          state]
    {:index (inc index),
     :available-fields (disj available-fields action),
     :fields-order (conj fields-order action),
     :used-fields (conj used-fields action)}))

(defn problem--simulate-greedy
  [problem state]
  (if (problem--is-final-state? problem state)
    [[state nil]]
    (first (for [action (problem--state-actions problem state)
                 :let [next-state (problem--next-state problem state action)
                       result (problem--simulate-greedy problem next-state)]
                 :when (some? result)]
             (lazy-seq (cons [state action] result))))))

(defn -main
  [& args]
  (let [data (->> (first args)
                  (slurp)
                  (parse-data))
        nearby-tickets-matches
          (->> (for [i (->> data
                            (:nearby-tickets)
                            (count)
                            (range))]
                 (for [j (->> data
                              (:fields)
                              (count)
                              (range))
                       :let [value (get-in (:nearby-tickets data) [i j])]]
                   (->> data
                        (:fields)
                        (filter #(field-validate (:constraint %) value))
                        (map :name)
                        (set))))
               (map vec)
               (vec))]
    ;; part 1
    (let [not-valid-values
            (for [i (->> data
                         (:nearby-tickets)
                         (count)
                         (range))
                  j (->> data
                         (:fields)
                         (count)
                         (range))
                  :let [value (get-in (:nearby-tickets data) [i j])
                        matches (get-in nearby-tickets-matches [i j])]
                  :when (= (count matches) 0)]
              value)]
      (println "part 1" (apply + not-valid-values)))
    ;; part 2
    (let [valid-nearby-tickets
            (->> (for [i (->> data
                              (:nearby-tickets)
                              (count)
                              (range))
                       :let [ticket (nth nearby-tickets-matches i)]
                       :when (->> ticket
                                  (map (fn [matches] (> (count matches) 0)))
                                  (every? true?)
                                  (true?))]
                   i)
                 (mapv #(nth nearby-tickets-matches %)))
          cols-valid-fields (->> valid-nearby-tickets
                                 (transpose)
                                 (mapv #(apply clojure.set/intersection %)))
          problem (problem--create (:fields data) cols-valid-fields)
          initial-state (problem--make-initial-state problem)
          problem-solution (vec (problem--simulate-greedy problem
                                                          initial-state))
          fields-order (->> problem-solution
                            (count)
                            (dec)
                            (nth problem-solution)
                            (first)
                            (:fields-order)
                            (vec))]
      (println "part 2"
               (->> data
                    (:own-ticket)
                    (map vector fields-order)
                    (filter #(some? (re-matches #"^departure.*" (first %))))
                    (map second)
                    (apply *))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 7: Handy Haversacks
;; url:       https://adventofcode.com/2020/day/7
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-04
;; execution: $ bash ./aoc2020/run.sh d07
;; example:
;;            $ bash ./aoc2020/run.sh d07
;;            part 1 248
;;            part 2 57281
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d07.main)

(defn parse-rule
  [rule-raw]
  (let [[_ outer-bag inner-bags-raw] (re-matches #"^(.*) bags contain (.*)$"
                                                 rule-raw)
        inner-bags (->> inner-bags-raw
                        (re-seq #"(\d+) (.*?) bags?[\.\,]")
                        (map reverse)
                        (map (fn [[k v]] [k (Integer/parseInt v)]))
                        (into {}))]
    {:rule-raw rule-raw, :outer-bag outer-bag, :inner-bags inner-bags}))

(defn build-adjacent-list
  [arcs]
  (->> arcs
       (group-by first)
       (map (fn [[k v]] [k (map second v)]))
       (into {})))

(defn build-bag-weighted-digraph
  [rules]
  (let [vertices (set (concat (map :outer-bag rules)
                              (->> rules
                                   (map :inner-bags)
                                   (apply concat)
                                   (map first))))
        weights
          (->> rules
               (map
                 (fn [rule]
                   (map vector (repeat (:outer-bag rule)) (:inner-bags rule))))
               (apply concat)
               (map (fn [[outer-bag [inner-bag count]]] [[outer-bag inner-bag]
                                                         count]))
               (into {}))
        arcs (->> weights
                  (map first)
                  (set))
        adjacent (build-adjacent-list arcs)]
    {:vertices vertices, :arcs arcs, :weights weights, :adjacent adjacent}))

(defn reverse-weighted-digraph
  [digraph]
  (let [vertices (:vertices digraph)
        arcs (->> digraph
                  (:arcs)
                  (map reverse)
                  (set))
        weights (->> digraph
                     (:weights)
                     (map (fn [[k v]] [(reverse k) v]))
                     (into {}))
        adjacent (build-adjacent-list arcs)]
    {:vertices vertices, :arcs arcs, :adjacent adjacent}))

(defn digraph-dfs-reachable-step
  [digraph current visited]
  (if (contains? visited current)
    #{current}
    (let [next-visited (conj visited current)
          adj (get (:adjacent digraph) current)
          rec-reachable
            (for [next adj]
              (digraph-dfs-reachable-step digraph next next-visited))
          reachable (apply clojure.set/union (conj rec-reachable (set adj)))]
      reachable)))

(defn digraph-dfs-reachable
  [digraph source]
  (digraph-dfs-reachable-step digraph source #{}))

(defn digraph-tree-size
  [digraph current]
  (let [adj (get (:adjacent digraph) current)
        rec-values (for [next adj
                              :let [weight (get (:weights digraph)
                                                [current next])]]
                          (* weight (+ (digraph-tree-size digraph next) 1)))
        subtree-size (apply + rec-values)]
    subtree-size))

(defn -main
  [& args]
  (let [rules (->> (first args)
                   (slurp)
                   (clojure.string/split-lines)
                   (map parse-rule)
                   (vec))
        bag-weighted-digraph (build-bag-weighted-digraph rules)]
    ;; (clojure.pprint/pprint bag-weighted-digraph)
    ;; part 1
    (let [reversed-bag-digraph (reverse-weighted-digraph bag-weighted-digraph)
          reachable (digraph-dfs-reachable reversed-bag-digraph "shiny gold")]
      (println "part 1" (count reachable)))
    ;; part 2
    (let [shiny-gold-tree-size (digraph-tree-size bag-weighted-digraph
                                                  "shiny gold")]
      (println "part 2" shiny-gold-tree-size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 21: Allergen Assessment
;; url:       https://adventofcode.com/2020/day/21
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-16
;; execution: $ bash ./aoc2020/run.sh d21 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d21 < ./aoc2020/d21/ex01.txt
;;            part 1 5
;;            part 2 lmxmxvkd,sqjhc,fvjkl
;;            $ bash ./aoc2020/run.sh d21 < ./aoc2020/d21/input.txt
;;            part 1 2078
;;            part 2 lmcqt,kcddk,npxrdnd,cfb,ldkt,fqpt,jtfmtpd,tsch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d21.main)

(defn parse-food
  [food-raw]
  (let [[_ ingredients-raw allergens-raw]
          (re-matches #"^(.*) \(contains (.*)\)$" food-raw)
        ingredients (set (clojure.string/split ingredients-raw #" "))
        allergens (set (clojure.string/split allergens-raw #", "))]
    {:ingredients ingredients, :allergens allergens}))

(defn- digraph-bip-max-match-dfs
  [adj rev-assignment visited v]
  (loop [[u & sink-vertices-tail] (get adj v)
         rev-assignment rev-assignment
         visited visited]
    (cond ;
      (nil? u) [rev-assignment visited false]
      (not (contains? visited u))
        (if (not (contains? rev-assignment u))
          [(assoc rev-assignment u v) (conj visited u) true]
          (let [[next-rev-assignment next-visited result]
                  (digraph-bip-max-match-dfs adj
                                             rev-assignment
                                             (conj visited u)
                                             (get rev-assignment u))]
            (assert (or (true? result) (false? result)))
            (if result
              [(assoc next-rev-assignment u v) next-visited true]
              (recur sink-vertices-tail rev-assignment next-visited))))
      :else (recur sink-vertices-tail rev-assignment visited))))

(defn digraph-bip-max-match
  "https://www.tutorialspoint.com/Maximum-Bipartite-Matching"
  [adj]
  (loop [[v & source-vertices-tail] (keys adj)
         rev-assignment {}]
    (if (nil? v)
      (clojure.set/map-invert rev-assignment)
      (recur source-vertices-tail
             (first (digraph-bip-max-match-dfs adj rev-assignment #{} v))))))

(defn -main
  [& args]
  (let [foods-doc (->> *in*
                       (slurp)
                       (clojure.string/split-lines)
                       (map parse-food)
                       (vec))
        ingredients (->> foods-doc
                         (map :ingredients)
                         (apply clojure.set/union))
        allergens (->> foods-doc
                       (map :allergens)
                       (apply clojure.set/union))
        allergens-candidate-ingredients
          (->> foods-doc
               (map (fn [{ingredients :ingredients, allergens :allergens}]
                      (for [allergen allergens] [allergen ingredients])))
               (apply concat)
               (group-by first)
               (map (fn [[allergen allergen-ingredients-sets]]
                      [allergen
                       (->> allergen-ingredients-sets
                            (map second)
                            (apply clojure.set/intersection))]))
               (into {}))
        ingredients-candidate-allergens
          (->> allergens-candidate-ingredients
               (map (fn [[allergen ingredients]]
                      (for [ingredient ingredients] [ingredient allergen])))
               (apply concat)
               (group-by first)
               (map (fn [[ingredient ingredient-allergens]]
                      [ingredient
                       (->> ingredient-allergens
                            (map second)
                            (set))]))
               (into {}))]
    ;; (clojure.pprint/pprint foods-doc)
    ;; part 1
    (let [ingredients-without-candidate-allergens
            (clojure.set/difference ingredients
                                    (->> ingredients-candidate-allergens
                                         (keys)
                                         (set)))]
      (println "part 1"
               (->> foods-doc
                    (map (fn [{ingredients :ingredients}] ingredients))
                    (map #(clojure.set/intersection
                             %
                             ingredients-without-candidate-allergens))
                    (map count)
                    (apply +))))
    ;; part 2
    (let [ingredient-allergen-matches (digraph-bip-max-match
                                        ingredients-candidate-allergens)]
      (println "part 2"
               (->> ingredient-allergen-matches
                    (seq)
                    (sort-by second)
                    (map first)
                    (clojure.string/join ","))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 22: Crab Combat
;; url:       https://adventofcode.com/2020/day/22
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-17
;; execution: $ bash ./aoc2020/run.sh d22 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d22 < ./aoc2020/d22/ex01.txt
;;            part 1 306
;;            part 2 291
;;            $ bash ./aoc2020/run.sh d22 < ./aoc2020/d22/input.txt
;;            part 1 35013
;;            part 2 32806
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d22.main)

(defn parse-game-doc
  [game-doc-raw]
  (let [[_ p1-deck-raw p2-deck-raw]
          (re-matches #"(?s)^Player 1:\n(.*?)\n\nPlayer 2:\n(.*?)\n$"
                      game-doc-raw)
        p1-deck (->> p1-deck-raw
                     (clojure.string/split-lines)
                     (map #(Integer/parseInt %))
                     (vec))
        p2-deck (->> p2-deck-raw
                     (clojure.string/split-lines)
                     (map #(Integer/parseInt %))
                     (vec))]
    (assert (= (+ (count p1-deck) (count p2-deck))
               (count (clojure.set/union (set p1-deck) (set p2-deck)))))
    {:p1-deck p1-deck, :p2-deck p2-deck}))

(defn p1-game-recursion
  [state]
  ;; (println state)
  (let [{round :round, p1-deck :p1-deck, p2-deck :p2-deck} state
        [p1-card & p1-deck-tail] p1-deck
        [p2-card & p2-deck-tail] p2-deck]
    (if (or (empty? p1-deck) (empty? p2-deck))
      state
      (if (> p1-card p2-card)
        (p1-game-recursion ;
          {:round (inc round),
           :p1-deck (conj (vec p1-deck-tail) p1-card p2-card),
           :p2-deck (vec p2-deck-tail)})
        (p1-game-recursion ;
          {:round (inc round),
           :p1-deck (vec p1-deck-tail),
           :p2-deck (conj (vec p2-deck-tail) p2-card p1-card)})))))

(defn p2-game-recursion
  [state]
  ;; (println (dissoc state :previous-card-states))
  (let [{p1-deck :p1-deck,
         p2-deck :p2-deck,
         previous-card-states :previous-card-states}
          state
        current-card-state {:p1-deck p1-deck, :p2-deck p2-deck}
        p1-card (first p1-deck)
        p2-card (first p2-deck)
        p1-deck-tail (if (> (count p1-deck) 1) (subvec p1-deck 1) (vec nil))
        p2-deck-tail (if (> (count p2-deck) 1) (subvec p2-deck 1) (vec nil))]
    (cond ;
      (empty? p1-deck) ;
        (assoc state
          :winner :p2
          :winner-deck p2-deck)
      (empty? p2-deck) ;
        (assoc state
          :winner :p1
          :winner-deck p1-deck)
      (contains? previous-card-states current-card-state) ;
        (assoc state
          :winner :p1
          :winner-deck p1-deck)
      (and (<= p1-card (count p1-deck-tail)) (<= p2-card (count p2-deck-tail)))
        (case (:winner
                (p2-game-recursion {:p1-deck (subvec p1-deck-tail 0 p1-card),
                                    :p2-deck (subvec p2-deck-tail 0 p2-card),
                                    :previous-card-states #{}}))
          :p1 (recur ;
                {:p1-deck (conj p1-deck-tail p1-card p2-card),
                 :p2-deck p2-deck-tail,
                 :previous-card-states (conj previous-card-states
                                             current-card-state)})
          :p2 (recur ;
                {:p1-deck p1-deck-tail,
                 :p2-deck (conj p2-deck-tail p2-card p1-card),
                 :previous-card-states (conj previous-card-states
                                             current-card-state)}))
      (> p1-card p2-card) ;
        (recur ;
          {:p1-deck (conj p1-deck-tail p1-card p2-card),
           :p2-deck p2-deck-tail,
           :previous-card-states (conj previous-card-states
                                       current-card-state)})
      (< p1-card p2-card) ;
        (recur ;
          {:p1-deck p1-deck-tail,
           :p2-deck (conj p2-deck-tail p2-card p1-card),
           :previous-card-states (conj previous-card-states
                                       current-card-state)})
      :else ;
        (assert false))))

(defn -main
  [& args]
  (let [game-doc (->> *in*
                      (slurp)
                      (parse-game-doc))]
    ;; (clojure.pprint/pprint game-doc)
    ;; part 1
    (let [last-state (p1-game-recursion {:round 1,
                                         :p1-deck (:p1-deck game-doc),
                                         :p2-deck (:p2-deck game-doc),
                                         :previous-card-states #{}})]
      (println "part 1"
               (->> [(:p1-deck last-state) (:p2-deck last-state)]
                    (filter not-empty)
                    (first)
                    (reverse)
                    (map vector (map inc (range)))
                    (map #(apply * %))
                    (apply +))))
    ;; part 2
    (let [last-state (p2-game-recursion {:p1-deck (:p1-deck game-doc),
                                         :p2-deck (:p2-deck game-doc),
                                         :previous-card-states #{}})]
      (println "part 2"
               (->> [(:p1-deck last-state) (:p2-deck last-state)]
                    (filter not-empty)
                    (first)
                    (reverse)
                    (map vector (map inc (range)))
                    (map #(apply * %))
                    (apply +))))))

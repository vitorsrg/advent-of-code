;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 14: Docking Data
;; url:       https://adventofcode.com/2020/day/14
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-09
;; execution: $ bash ./aoc2020/run.sh d14 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d14 d14/ex02.txt
;;            part 1 51
;;            part 2 208
;;            $ bash ./aoc2020/run.sh d14
;;            part 1 9615006043476
;;            part 2 4275496544925
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d14.main)

(defn replace-many
  [content & replacements]
  (let [replacement-list (partition 2 replacements)]
    (reduce #(apply clojure.string/replace %1 %2) content replacement-list)))

(defn parse-action
  [action-raw]
  (if (re-matches #"^mask.*$" action-raw)
    (let [[_ mask] (re-matches #"^mask = (\w+)$" action-raw)]
      {:type :mask,
       :mask mask,
       :mask-1 (Long/parseLong (clojure.string/replace mask "X" "0") 2),
       :mask-0 (Long/parseLong (clojure.string/replace mask "X" "1") 2)})
    (let [[_ address value] (re-matches #"^mem\[(\d+)\] = (\d+)$" action-raw)]
      {:type :mem,
       :address (Long/parseLong address),
       :value (Long/parseLong value)})))

(defn markov-simulate
  [transition initial-state actions]
  (reductions transition initial-state actions))

(defn p1-program-next-state
  [state action]
  (case (:type action)
    :mask (assoc state
            :mask (:mask action)
            :mask-1 (:mask-1 action)
            :mask-0 (:mask-0 action))
    :mem (->> (:value action)
              (bit-or (:mask-1 state))
              (bit-and (:mask-0 state))
              (assoc-in state [:mem (:address action)]))))

(defn product-colls
  [colls]
  "https://rosettacode.org/wiki/Cartesian_product_of_two_or_more_lists#Clojure"
  (if (empty? colls)
    '(())
    (for [more (product-colls (rest colls)) x (first colls)] (cons x more))))

(defn get-floating-addresses
  [mask address]
  (let [bincode (Long/toString address 2)
        bincode36 (-> (- 36 (count bincode))
                      (repeat "0")
                      (concat bincode)
                      (clojure.string/join))]
    (->> (map vector mask bincode36)
         (map (fn [[m v]] (if (= m \0) v m)))
         (map (fn [v] (if (= v \X) [0 1] [v])))
         (product-colls)
         (map #(apply str %))
         (map #(Long/parseLong % 2))
         (vec))))

(defn p2-program-next-state
  [state action]
  (case (:type action)
    :mask (assoc state :mask (:mask action))
    :mem (reduce (fn [state floating-address]
                   (assoc-in state [:mem floating-address] (:value action)))
           state
           (get-floating-addresses (:mask state) (:address action)))))

(defn -main
  [& args]
  (let [actions (->> (first args)
                     (slurp)
                     (clojure.string/split-lines)
                     (map parse-action)
                     (vec))]
    ;; (clojure.pprint/pprint actions)
    ;; part 1
    (let [states (vec (markov-simulate p1-program-next-state {:mem {}} actions))
          last-state (nth states (count actions))]
      ;; (clojure.pprint/pprint states)
      (println "part 1"
               (->> last-state
                    (:mem)
                    (vals)
                    (apply +))))
    ;; part 2
    (let [states (vec (markov-simulate p2-program-next-state {:mem {}} actions))
          last-state (nth states (count actions))]
      ;; (clojure.pprint/pprint states)
      (println "part 1"
               (->> last-state
                    (:mem)
                    (vals)
                    (apply +))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 19: Monster Messages
;; url:       https://adventofcode.com/2020/day/19
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-13
;; execution: $ bash ./aoc2020/run.sh d19 < [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d19 < ./aoc2020/d19/ex02.txt
;;            part 1 3
;;            part 2 12
;;            $ bash ./aoc2020/run.sh d19 < ./aoc2020/d19/input.txt
;;            part 1 269
;;            part 2 403
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d19.main)

(defn parse-rule
  [data-raw]
  (cond ;
    (some? (re-matches #"^\"[a-z]\"$" data-raw)) ;
      {:type :adhoc,
       :data (->> data-raw
                  (re-matches #"^\"([a-z])\"$")
                  (second))}
    (clojure.string/includes? data-raw "|") ;
      {:type :or,
       :data (->> (clojure.string/split data-raw #"\|")
                  (map parse-rule)
                  (vec))}
    (some? (re-matches #"^\s*(\d+(?:\s+\d+)*)\s*$" data-raw)) ;
      {:type :concat,
       :data (->> data-raw
                  (re-seq #"\d+")
                  (mapv #(Integer/parseInt %)))}))

(defn parse-data
  [data-raw]
  (let [[_ rules-raw msgs-raw] (re-matches #"(?s)^(.*?)\n\n(.*?)$" data-raw)
        rules (->> rules-raw
                   (clojure.string/split-lines)
                   (map #(re-matches #"^(\d+): (.*)$" %))
                   (map rest)
                   (map (fn [[key-raw rule-raw]] [(Integer/parseInt key-raw)
                                                  (parse-rule rule-raw)]))
                   (into {}))
        msgs (->> msgs-raw
                  (clojure.string/split-lines)
                  (vec))]
    {:rules rules, :msgs msgs}))

(defn build-dag-rule
  [rules root-rule-key]
  (let [rec-fn (fn [memo-rec-fn rkey]
                 (let [{rtype :type, rdata :data} (get rules rkey)]
                   (case rtype
                     :adhoc rdata
                     :or (->> rdata
                              (map (fn [concat-rule]
                                     (->> concat-rule
                                          (:data)
                                          (map #(memo-rec-fn memo-rec-fn %))
                                          (apply str))))
                              (clojure.string/join "|")
                              (#(apply str "(?:" % ")")))
                     :concat (->> rdata
                                  (map #(memo-rec-fn memo-rec-fn %))
                                  (apply str)))))
        memo-rec-fn (memoize rec-fn)]
    (->> (memo-rec-fn memo-rec-fn root-rule-key)
         (#(apply str "^" % "$")))))

(defn collapse-raw-lazy-rule
  [lazy-rule]
  (let [[[t1 d1] [t2 d2] & tail] lazy-rule]
    (cond ;
      (nil? t1) ;
        []
      (nil? t2) ;
        [[t1 d1]]
      (and (= t1 :raw) (= t2 :raw)) ;
        (->> tail
             (cons [:raw (str d1 d2)])
             (collapse-raw-lazy-rule)
             (lazy-seq))
      :else ;
        (->> tail
             (cons [t2 d2])
             (collapse-raw-lazy-rule)
             (cons [t1 d1])
             (lazy-seq)))))

(defn build-lazy-rule-dfs
  [rules root-rule-key]
  (let [rec-fn
          (fn rec-fn [visited rkey]
            (if (contains? visited rkey)
              [[:ref rkey]]
              (let [{rtype :type, rdata :data} (get rules rkey)]
                (case rtype
                  :adhoc [[:raw rdata]]
                  :or (->> rdata
                           (map (fn [concat-rule]
                                  (->> concat-rule
                                       (:data)
                                       (map #(rec-fn (conj visited rkey) %))
                                       (apply concat))))
                           (interleave (repeat [[:raw "|"]]))
                           (rest)
                           (apply concat)
                           (#(apply concat [[:raw "(?:"]] % [[:raw ")"]] [])))
                  :concat (->> rdata
                               (map #(rec-fn (conj visited rkey) %))
                               (apply concat))))))]
    (->> (rec-fn #{} root-rule-key)
         (collapse-raw-lazy-rule)
         (vec))))

(defn expand-lazy-rules-once
  [lazy-rules]
  (let [expand-lazy-rule-once
          (fn [rkey]
            (->> rkey
                 (get lazy-rules)
                 (map (fn [[t d]] (if (= t :raw) [[t d]] (get lazy-rules d))))
                 (apply concat)
                 (collapse-raw-lazy-rule)
                 (vec)))]
    (->> lazy-rules
         (map (fn [[rkey lazy-rule]] [rkey (expand-lazy-rule-once rkey)]))
         (into {}))))

(defn prune-lazy-rule
  [lazy-rule]
  (->> lazy-rule
       (#(lazy-cat [[:raw "^"]] % [[:raw "$"]]))
       (filter (fn [[t d]] (= t :raw)))
       (map second)
       (apply str)))

(defn -main
  [& args]
  (let [data (->> *in*
                  (slurp)
                  (parse-data))
        {rules :rules, msgs :msgs} data]
    ;; (clojure.pprint/pprint data)
    ;; part 1
    (let [regex-rule (build-dag-rule rules 0)
          msgs-matches-count (->> msgs
                                  (map #(re-matches (re-pattern regex-rule) %))
                                  (filter some?)
                                  (count))]
      (println "part 1" msgs-matches-count))
    ;; part 2
    (let [rules-override (assoc rules
                           8 (parse-rule "42 | 42 8")
                           11 (parse-rule "42 31 | 42 11 31"))
          lazy-rules (->> rules-override
                          (keys)
                          (map (fn [rkey] [rkey
                                           (build-lazy-rule-dfs rules-override
                                                                rkey)]))
                          (into {}))
          lazy-rule-expansions 8 ;; which causes a 2••x expansion depth
          lazy-rules-expanded (reduce (fn [lazy-rules _]
                                        (expand-lazy-rules-once lazy-rules))
                                lazy-rules
                                (range lazy-rule-expansions))
          regex-rule (->> 0
                          (get lazy-rules-expanded)
                          (prune-lazy-rule))
          msgs-matches-count (->> msgs
                                  (map #(re-matches (re-pattern regex-rule) %))
                                  (filter some?)
                                  (count))]
      (println "part 2" msgs-matches-count))))

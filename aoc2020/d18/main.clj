;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; domain:    Advent of Code 2020
;; challenge: Day 18:
;; url:       https://adventofcode.com/2020/day/18
;; author:    Vitor SRG (vitorssrg@gmail.com)
;; date:      2021-01-12
;; execution: $ bash ./aoc2020/run.sh d18 [INPUT_FILE]
;; example:
;;            $ bash ./aoc2020/run.sh d18 < ./aoc2020/d18/input.txt
;;            part 1 6640667297513
;;            part 2 451589894841552
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns d18.main)

(defn parse-expression
  [expression-raw]
  (->> expression-raw
       (re-seq #"(?:\d+|\+|\*|\(|\))")
       (map (fn [token]
              (if (some? (re-matches #"^\d+$" token))
                (Integer/parseInt token)
                (nth token 0))))
       (vec)))

(defn nest-parens
  [expression]
  (let [[token & tail] expression]
    (case token
      nil [nil nil]
      \( (let [[group subtail] (nest-parens tail)
               [next-group next-subtail] (nest-parens subtail)]
           [(lazy-seq (cons group next-group)) next-subtail])
      \) [[] tail]
      (let [[group subtail] (nest-parens tail)]
        [(lazy-seq (cons token group)) subtail]))))

(defn p1-eval-expr
  [expression]
  ;; (clojure.pprint/pprint expression)
  (if (number? expression)
    expression
    (let [[subexpr1 op subexpr2 & subtail] expression]
      (cond ;
        (nil? subexpr1) ;
          nil
        (nil? op) ;
          (p1-eval-expr subexpr1)
        (= op \+) ;
          (p1-eval-expr (cons (+ (p1-eval-expr subexpr1)
                                 (p1-eval-expr subexpr2))
                              subtail))
        (= op \*) ;
          (p1-eval-expr (cons (* (p1-eval-expr subexpr1)
                                 (p1-eval-expr subexpr2))
                              subtail))))))

(defn p2-eval-expr
  [expression]
  ;; (clojure.pprint/pprint expression)
  (if (number? expression)
    expression
    (let [[subexpr1 op1 subexpr2 op2 subexpr3 & subtail] expression]
      (cond ;
        (nil? subexpr1) ;
          nil
        (nil? op1) ;
          (p2-eval-expr subexpr1)
        (= op1 \+) ;
          (p2-eval-expr
            (concat [(+ (p2-eval-expr subexpr1) (p2-eval-expr subexpr2)) ;
                     op2 ;
                     subexpr3]
                    subtail))
        (= op2 \+) ;
          (p2-eval-expr
            (concat [subexpr1 ;
                     op1 ;
                     (+ (p2-eval-expr subexpr2) (p2-eval-expr subexpr3))]
                    subtail))
        (= op1 \*) ;
          (p2-eval-expr
            (concat [(* (p2-eval-expr subexpr1) (p2-eval-expr subexpr2)) ;
                     op2 ;
                     subexpr3]
                    subtail))))))

(def sample-expressions
  [;
   "1" ;
   "(1)" ;
   "((1))" ;
   "1 + 2" ;
   "1 + 2 * 3" ;
   "1 + (2 * 3)" ;
   "((1 + 2) * (3 + 4))" ;
  ] ;
)

(def example-expressions
  [;
   "1 + 2 * 3 + 4 * 5 + 6" ;
   "2 * 3 + (4 * 5)" ;
   "5 + (8 * 3 + 9 + 3 * 4 * 3)" ;
   "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" ;
   "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" ;
  ] ;
)

(defn -main
  [& args]
  (let [expressions (->> *in*
                         (slurp)
                         (clojure.string/split-lines)
                         (map parse-expression)
                         (vec))]
    ;; (clojure.pprint/pprint expressions)
    ;; part 1
    ;; (->> example-expressions
    ;;      (map parse-expression)
    ;;      (map nest-parens)
    ;;      (map first)
    ;;      (map p1-eval-expr)
    ;;      (vec)
    ;;      (map clojure.pprint/pprint)
    ;;      (vec))
    (println "part 1"
             (->> expressions
                  (map nest-parens)
                  (map first)
                  (map p1-eval-expr)
                  (apply +)))
    ;; part 2
    ;; (->> example-expressions
    ;;      (map parse-expression)
    ;;      (map nest-parens)
    ;;      (map first)
    ;;      (map p2-eval-expr)
    ;;      (vec)
    ;;      (map clojure.pprint/pprint)
    ;;      (vec))
    (println "part 2"
             (->> expressions
                  (map nest-parens)
                  (map first)
                  (map p2-eval-expr)
                  (apply +)))))

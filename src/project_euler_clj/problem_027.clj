;; Problem 27
;; <p>Euler discovered the remarkable quadratic formula:</p>
;; <p class="center">$n^2 + n + 41$</p>
;; <p>It turns out that the formula will produce 40 primes for the consecutive integer values $0 \le n \le 39$. However, when $n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41$ is divisible by 41, and certainly when $n = 41, 41^2 + 41 + 41$ is clearly divisible by 41.</p>
;; <p>The incredible formula $n^2 - 79n + 1601$ was discovered, which produces 80 primes for the consecutive values $0 \le n \le 79$. The product of the coefficients, −79 and 1601, is −126479.</p>
;; <p>Considering quadratics of the form:</p>
;; <blockquote>
;; $n^2 + an + b$, where $|a| &lt; 1000$ and $|b| \le 1000$<br /><br /><div>where $|n|$ is the modulus/absolute value of $n$<br />e.g. $|11| = 11$ and $|-4| = 4$</div>
;; </blockquote>
;; <p>Find the product of the coefficients, $a$ and $b$, for the quadratic expression that produces the maximum number of primes for consecutive values of $n$, starting with $n = 0$.</p>

(ns project-euler-clj.problem-027
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(defn monic-quadratic-func
  [a b]
  (fn [n] (+ (* n n) (* a n) b)))

(defn lazy-seq-gen
  ([func] (lazy-seq-gen func 0))
  ([func n] (cons (func n) (lazy-seq (lazy-seq-gen func (inc n))))))

(defn take-upto-non-prime
  [seq]
  (take-while (fn [n] (or (= n 0)
                          (= 1 (count (common/prime-factor (abs n))))))
              seq))


(defn solve []
  (take-last 1
             (sort-by (fn [[m n prime-seq]] (count prime-seq))
                      (for [m (range -1000 1000)
                            n (concat (common/memo-primes-upto 1000)
                                      (map #(* -1 %1) (common/memo-primes-upto 1000)))]
                        [m n (take-upto-non-prime (lazy-seq-gen (monic-quadratic-func m n)))]))))

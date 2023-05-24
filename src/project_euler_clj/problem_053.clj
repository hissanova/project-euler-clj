;; Problem 53

;; <p>There are exactly ten ways of selecting three from five, 12345:</p>
;; <p class="center">123, 124, 125, 134, 135, 145, 234, 235, 245, and 345</p>
;; <p>In combinatorics, we use the notation, $\displaystyle \binom 5 3 = 10$.</p>
;; <p>In general, $\displaystyle \binom n r = \dfrac{n!}{r!(n-r)!}$, where $r \le n$, $n! = n \times (n-1) \times ... \times 3 \times 2 \times 1$, and $0! = 1$.
;; </p>
;; <p>It is not until $n = 23$, that a value exceeds one-million: $\displaystyle \binom {23} {10} = 1144066$.</p>
;; <p>How many, not necessarily distinct, values of $\displaystyle \binom n r$ for $1 \le n \le 100$, are greater than one-million?</p>

(ns project-euler-clj.problem-053
  (:require [project-euler-clj.common :as common]))

(defn comb
  [n r]
  (/ (common/factorial n) (* (common/factorial r)
                             (common/factorial (- n r)))))


(defn solve []
  (count (filter #(< (bigint 1e6) %)
                 (flatten (map (fn [n] (map (fn [r] (comb (bigint n) (bigint r)))
                                            (range 1 (inc n))))
                               (range 1 101))))))

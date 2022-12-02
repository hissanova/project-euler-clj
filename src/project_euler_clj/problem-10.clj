;; <p>The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.</p>
;; <p>Find the sum of all the primes below two million.</p>
(ns project-euler-clj.problem-10)
(require '[project-euler-clj.common :as common])

(reduce + (common/get-primes-upto 2e6))

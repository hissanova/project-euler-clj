;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?
(ns project-euler-clj.problem-3)
(require '[project-euler-clj.common :as common])

(do (def target 600851475143)
    (def prime-limit (int (Math/sqrt target))))

(every? #(not= (mod 7 %) 0) [2 3 5])
(common/get-primes-upto 10)

(println (common/prime-factor 600851475143))
(reduce * (map #(Math/pow (:factor %) (:exp %)) [{:factor 2 :exp 3}
                                                 {:factor 2 :exp 3}]))
(reduce * (map #(Math/pow (:factor %) (:exp %)) (common/prime-factor 600851475143)))



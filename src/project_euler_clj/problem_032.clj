;; Problem 32
;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

;; HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

;; When a * b = c, where a, b, c are positive integers, their nums of digits are d_{a,b,c} = ceil(log_10(a,b,c))
;; then, we want  certainly we have
;; d_a + d_b + d_c = 9
;; But, because d_c = ceil(log_10(a) + log_10(b)), it must be that
;; (1) d_a + d_b = d_c
;; or
;; (2) d_a + d_b = d_c + 1
;; which lead to
;; (1)' d_a + d_b = 4.5
;; and
;; (2)' d_a + d_b = 5.
;; But (1)' is impossible since d's are integers. Hence we must have d_a + d_b = 5
;; So we only have to search for the cases with (d_a, d_b) = (1, 4) or (2, 3).


(ns project-euler-clj.problem-032
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:require [clojure.math.combinatorics :as combo]))

(def digits-pairs-1-4 (for [m (range 1 10)
                            n (range 1000 10000)]
                        [m n]))
(def digits-pairs-2-3 (for [m (range 10 100)
                            n (range 100 1000)]
                        [m n]))

(def digit-pairs (concat digits-pairs-1-4 digits-pairs-2-3))
(count digit-pairs)
(def candidates (combo/permuted-combinations (range 1 10) 5))

(defn- produce-candidate-pair-part
  [five-digits]
  (let [candidates [(split-at 2 five-digits)]]
    (if (= 1 (first five-digits))
      candidates
      (conj candidates (split-at 1 five-digits)))))


(defn criterion
  [[a b]]
  (let [c (apply * (map common/digit-seq-to-num [a b]))]
    (when (= 4 (int (Math/ceil (Math/log10 c))))
      (let [c-digits (remove #{0} (map :digit ((common/num-to-digits c) :body)))]
        (when (= 9 (count (set (concat a b c-digits))))
          [a b c-digits])))))

(criterion ['(3 9) '(1 8 6)])
(map criterion (produce-candidate-pair-part [3 9 1 8 6]))

(def pandigital-triples (filter not-empty
                                (map (comp #(apply concat %)
                                           (fn [x] (map criterion
                                                        (produce-candidate-pair-part x))))
                                     candidates)))

(defn solve []
  (reduce + (set (map #(common/digit-seq-to-num (nth % 2))
                      pandigital-triples))))

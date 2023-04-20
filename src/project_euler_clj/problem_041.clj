;; Problem 41
;; We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

;; What is the largest n-digit pandigital prime that exists?

(ns project-euler-clj.problem-041
  (:require [project-euler-clj.common :as common])
  (:require [clojure.math.combinatorics :as combo]))

(def possible-nums-of-digits (filter (fn [n] (not= 0 (-> n
                                                        (#(/ (* % (inc %)) 2))
                                                        (rem 3))))
                                    (range 2 10)))
(defn gen-n-pandigital-nums
  [n]
  (combo/permutations (range 1 (inc n))))

(defn solve
  []
  (last (filter common/is-prime?
                (map common/digit-seq-to-num
                     (apply concat
                            (map gen-n-pandigital-nums
                                 possible-nums-of-digits))))))

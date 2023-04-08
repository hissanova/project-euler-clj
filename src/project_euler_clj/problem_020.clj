;; Problem 20
;; n! means n × (n − 1) × ... × 3 × 2 × 1

;; For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;; and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

;; Find the sum of the digits in the number 100!


(ns project-euler-clj.problem-20
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn add-digits [n]
  (loop
      [dividend n
       sum 0]
    (if (< (Math/log10 dividend) 1)
      (+ sum dividend)
      (recur (bigint (/ dividend 10))
             (+ sum (mod dividend 10))))))

(defn solve []
  (add-digits (common/factorial 100N)))
(solve)

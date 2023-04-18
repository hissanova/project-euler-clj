;; Problem 36
;; The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;; (Please note that the palindromic number, in either base, may not include leading zeros.)


(ns project-euler-clj.problem-036
  (:require [project-euler-clj.common :as common]))

(defn candidates
  [limit]
  (filter odd?
          (range limit)))

(defn is-palindromic?
  [seq]
  (= seq (reverse seq)))

(defn is-double-palindromic?
  [n base1 base2]
  (every? #(is-palindromic? (common/num-to-digit-seq n %))
          [base1 base2]))

(defn solve [limit]
  (reduce + (filter #(is-double-palindromic? % 10 2)
                    (candidates limit))))

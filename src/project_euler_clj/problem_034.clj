;; Problem 34
;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

;; Note: As 1! = 1 and 2! = 2 are not sums they are not included.

(ns project-euler-clj.problem-034
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(def max-digits (last (take-while #(< (int (Math/pow 10 (dec %)))
                                      (* % (common/factorial 9)))
                                  (range 1 100))))

(defn solve []
  (reduce + (filter (fn [n] (= n (reduce + (map common/factorial
                                                (map :digit
                                                     (:body (common/num-to-digits n)))))))
                    (flatten (map (fn [n] (common/gen-n-digits-nums n))
                                  (range 2 (inc max-digits)))))))

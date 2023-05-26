;; Problem 56
;; <p>A googol (10<sup>100</sup>) is a massive number: one followed by one-hundred zeros; 100<sup>100</sup> is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.</p>
;; <p>Considering natural numbers of the form, <i>a<sup>b</sup></i>, where <i>a, b</i> &lt; 100, what is the maximum digital sum?</p>
(ns project-euler-clj.problem-056
  (:require [project-euler-clj.common :as common]))

(defn sum-digits
  [n]
  (reduce + (common/num-to-digit-seq n)))

(apply max (map second
                (for [a (range 1 100)
                      b (range 1 100)]
                  (let [pow (common/pow (bigint a) b)]
                   [[a b pow] (sum-digits pow)]))))

(defn solve
  [mx]
  (apply max-key
         (cons second
             (for [a (range 1 mx)
                   b (range 1 100)]
               (let [pow (common/pow (bigint a) b)]
                 [[a b pow] (sum-digits pow)])))))

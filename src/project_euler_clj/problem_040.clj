;; Problem 39
;; <p>An irrational decimal fraction is created by concatenating the positive integers:</p>
;; <p class="center">0.12345678910<span class="red strong">1</span>112131415161718192021...</p>
;; <p>It can be seen that the 12<sup>th</sup> digit of the fractional part is 1.</p>
;; <p>If <i>d</i><sub><i>n</i></sub> represents the <i>n</i><sup>th</sup> digit of the fractional part, find the value of the following expression.</p>
;; <p class="center"><i>d</i><sub>1</sub> × <i>d</i><sub>10</sub> × <i>d</i><sub>100</sub> × <i>d</i><sub>1000</sub> × <i>d</i><sub>10000</sub> × <i>d</i><sub>100000</sub> × <i>d</i><sub>1000000</sub></p>

(ns project-euler-clj.problem-040
  (:require [project-euler-clj.common :as common]))

(defn pow
  [base p]
  (reduce * (repeat p base)))

(defn num-of-digits-func*
  ([] (num-of-digits-func* 0 0))
  ([num-of-digits total-num]
   (cons total-num
         (lazy-seq (num-of-digits-func* (inc num-of-digits)
                                        (+ total-num
                                           (* 9
                                              (inc num-of-digits)
                                              (pow 10 num-of-digits))))))))

(defn decimal-pos-to-digit-nums
  [deci-pos]
  (take-while #(< % deci-pos)  (num-of-digits-func*)))

(defn additive-inverse-mod
  [n modulus]
  (-> n
      (rem modulus)
      (#(- modulus %))
      (rem modulus)))

(defn decimal-pos-to-digit
  [dec-pos]
  (let [digit-nums (decimal-pos-to-digit-nums dec-pos)
        digits (count digit-nums)
        remain (- dec-pos (last digit-nums))]
    [(+ (dec (pow 10 (dec digits)))
        (int (Math/ceil (/ remain digits))))
     (additive-inverse-mod remain digits)]))

(defn get-digit-at
  [n pos]
  (-> n
      (common/num-to-digit-seq)
      (reverse)
      (vec)
      (get pos)))

(defn solve
  []
  (reduce * (map #(apply get-digit-at %)
                 (map decimal-pos-to-digit
                      [1 10 100 1000 10000 100000 1000000]))))

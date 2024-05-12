;; Problem 80
;; <p>It is well known that if the square root of a natural number is not an integer, then it is irrational. The decimal expansion of such square roots is infinite without any repeating pattern at all.</p>
;; <p>The square root of two is $1.41421356237309504880\cdots$, and the digital sum of the first one hundred decimal digits is $475$.</p>
;; <p>For the first one hundred natural numbers, find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.</p>

(ns project-euler-clj.problem-080
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn get-next-decimal-digit
  [n k]
  (let [n_ (* 10 n)
        pow (int (Math/log10 n_))
        digit-pos (common/pow 10N pow)]
    (last (take-while #(> (* k digit-pos)
                          (quot (common/pow (+ n_ %) 2)
                                digit-pos))
                      (range 10)))))

(defn sqrt-to-kth-pos
  [n k]
  (loop
      [digits (bigint (int (Math/sqrt n)))
       pos 0]
    (if (= pos k)
      digits
      (recur
       (+ (* digits 10)
          (get-next-decimal-digit digits n))
       (inc pos)))))

(defn solve []
  (apply +
         (map #(apply +
                      (common/num-to-digit-seq (sqrt-to-kth-pos % 99)))
              (filter (fn [x]
                        (not= 0.0 (mod (Math/sqrt x) 1.0)))
                      (range 1 100)))))



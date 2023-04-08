;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

;; What is the sum of the digits of the number 2^1000?

(ns project-euler-clj.problem-16
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn bigint?
  [n]
  (instance? bigint n))

(bigint? 1)

(defn int-pow
  [a b]
  (if (or (not (and  (<= 0 a)
                     (<= 0 b))))
    (throw (Exception. (format "Args must be positive integers a=%l, b=%d" a b))))
  (when (= b 0)
    1)
  (when (= b 1)
    a)
  (reduce * (repeat b a)))

(def the-int (int-pow 2N 1000))
(Math/log10 the-int)

(defn serialise-nat-num
  [n]
  (loop [k n
         res []]
    (let [q (quot k 10)]
      (if (= q 0)
        (conj res k)
        (recur q (concat [(mod k 10)] res))))))

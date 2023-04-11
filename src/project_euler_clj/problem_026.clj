;; Problem 25
;; A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

;; 1/2	= 	0.5
;; 1/3	= 	0.(3)
;; 1/4	= 	0.25
;; 1/5	= 	0.2
;; 1/6	= 	0.1(6)
;; 1/7	= 	0.(142857)
;; 1/8	= 	0.125
;; 1/9	= 	0.(1)
;; 1/10	= 	0.1
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

;; Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

(ns project-euler-clj.problem-026
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(defn split-to-digits
  [n base exp]
  (map #(int (mod (/ n (Math/pow base %1)) base)) (reverse (range exp))))

(defn split-n-flatten
  [decimal-digits-seq exp]
  (flatten (map (fn [n] (split-to-digits n 10 exp))
                                  decimal-digits-seq)))

(defn canonical-form
  [decimal-expr modulus]
  (let [expo (int (Math/log10 modulus))]
    (let [new-principal-v (split-n-flatten (rest (:principal-value decimal-expr))
                                           expo)
          new-cycle (split-n-flatten (:cycle decimal-expr)
                                     expo)
          digits-in-cycle (set new-cycle)]
      (if (= (first (reverse new-principal-v))
             (first (reverse new-cycle)))
        (let [new-principal-v' (take (dec (count new-principal-v))
                                     new-principal-v)
              new-cycle' (concat (take-last 1
                                            new-cycle)
                                 (take (dec (count new-cycle))
                                       new-cycle))]
          (if (= 1 (count (set new-cycle')))
            {:principal-value (cons 0 new-principal-v')
             :cycle (take-last 1 new-cycle')}
            {:principal-value (cons 0 new-principal-v')
             :cycle new-cycle'}))
        {:principal-value (cons 0 new-principal-v)
         :cycle new-cycle}))))

(defn fraction-decimal-exp
  "Finds the decimal expression for the recipraocal fraction 1/n of a given natural number n with a cycle."
  [n]
  (when (= n 0) (throw (Exception. "You can't divide by 0.")))
  (if (= n 1)
    {:principal-value 1 :cycle '(0)}
    (let [modulus (int (Math/pow 10 (Math/ceil (Math/log10 n))))]
      (loop [decimal-digits [0]
             remainder 1]
        (let [new-rem (int (mod (* modulus remainder) n))
              quot (quot (* modulus remainder) n)
              repeated-index (.indexOf decimal-digits quot)]
               (cond
                 (= new-rem 0) {:principal-value (seq (conj decimal-digits quot)) :cycle nil}
                 (and (> quot 0)
                      (> repeated-index 0)) (canonical-form {:principal-value (take repeated-index decimal-digits)
                                              :cycle (take-last (- (count decimal-digits) repeated-index)
                                                                decimal-digits)}
                                                            modulus)
                 :else (recur (conj decimal-digits quot)
                              new-rem)))))))

(defn solve []
  (take-last 1
             (sort-by (fn [x] (nth x 1))
                      (map (fn [n] [n (count (:cycle (fraction-decimal-exp n)))])
                           (range 1 1000)))))

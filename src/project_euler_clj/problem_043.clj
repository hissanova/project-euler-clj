;; Problem 43
;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.

;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:

;; d2d3d4=406 is divisible by 2
;; d3d4d5=063 is divisible by 3
;; d4d5d6=635 is divisible by 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11
;; d7d8d9=728 is divisible by 13
;; d8d9d10=289 is divisible by 17
;; Find the sum of all 0 to 9 pandigital numbers with this property.

;; Consideration:
;; d_4 must be in {0, 2, 4, 6, 8}
;; d_3 + d_4 + d_5 must be divisible by 3
;; d_6 must be either 0 or 5
;; d_6 - d_7 + d_8 = 0 mod 11.
;; When d_6 - d_7 + d_8 = 0
;; then d6 = 5, d_7 = d_8 + 5,  d_8 is in {1,2,3,4}.
;; Because d_7 is in {1,3,5,7,9}, possible (d_7, d_8) are (7,2), (9,4)
;; then d_6d_7d_8 = 572 or 594, which are both divisible by 11.
;; Consider when d_6 - d_7 + d_8 = 11n for some n > 0.
;; We see that max{d_6 - d_7 + d_8} <= 17, so n = 1.
;; Then d_6 - d_7 + d_8 = 11 and d6 = 5.
;; So - d_7 + d_8 = 6. Hence candidates are (d_6,d_7,d_8) = (5,1,7),(5,3,9)

(ns project-euler-clj.problem-043
  (:require [project-euler-clj.common :as common])
  (:require [clojure.set :as sets])
  (:require [clojure.math.combinatorics :as combo]))

(defn legal-d678?
  [d6 d7 d8]
  (= 0 (rem  (common/digit-seq-to-num [d6 d7 d8]) 11)))

(def legal-d-678 (filter (fn [triple] (and (some #{(first triple)} [0 5])
                                           (apply legal-d678? triple)))
                         (combo/permuted-combinations (range 10) 3)))

(defn legal-for-d9?
  [d7 d8 d9]
  (= 0 (rem (common/digit-seq-to-num [d7 d8 d9]) 13)))

(filter #(legal-for-d9? 7 2 %) (range 10))

(defn legal-for-d10?
  [d8 d9 d10]
  (= 0 (rem (common/digit-seq-to-num [d8 d9 d10]) 17)))

(defn legal-for-d5?
  [d5 d6 d7]
  (= 0 (rem (common/digit-seq-to-num [d5 d6 d7]) 7)))

(defn legal-d345?
  [d3 d4 d5]
  (= 0 (rem (+ d3 d4 d5) 3)))

(defn gen-possible-digits
  [tuple-poss-digs-seq concat-func]
  (apply concat
         (map (fn [[tuple poss-digits]] (map (fn [n] (concat-func n tuple))
                                             poss-digits))
              (filter (fn [[_ poss-digits]] (< 0 (count poss-digits)))
                      tuple-poss-digs-seq))))
(defn- concat-tail
  [n v]
  (conj v n))

(defn- concat-head
  [n v]
  (vec (cons n v)))

(def legal-d6789 (gen-possible-digits (map (fn [[[d6 d7 d8] digits]] [[d6 d7 d8]
                                                                      (filter (fn [d9] (legal-for-d9? d7 d8 d9))
                                                                              digits)])
                                           (map (fn [d678] [d678 (remove (set d678) (range 10))])
                                                legal-d-678))
                                      concat-tail))

(def legal-d678910 (gen-possible-digits (map (fn [[d6 d7 d8 d9]] [[d6 d7 d8 d9]
                                                                  (->> (range 10)
                                                                       (filter  (fn [n] (not-any? #{n} [d6 d7 d8 d9])))
                                                                       (filter (fn [d10] (legal-for-d10? d8 d9 d10))))])
                                             legal-d6789)
                                        concat-tail))

(def legal-d5678910 (gen-possible-digits (map (fn [d678910] [d678910
                                                             (->> (range 10)
                                                                  (filter (fn [n] (not-any? #{n} d678910)))
                                                                  (filter (fn [d5] (legal-for-d5? d5 (first d678910) (second d678910)))))])
                                              legal-d678910)
                                         concat-head))

(def legal-d1234 (map (fn [d5678910] [(vec (remove (set d5678910) (range 10)))
                                      d5678910])
                      legal-d5678910))

(defn solve
  []
  (reduce +
          (map common/digit-seq-to-num
               (filter (fn [sq] (and (not= 0 (first sq))
                                     (apply legal-d345?
                                            (take-last 3 (take 5 sq)))
                                     (even? (nth sq 3))))
                       (apply concat
                              (map (fn [[d1234 d5678910]] (map (fn [x] (concat x d5678910))
                                                               (combo/permutations d1234)))
                                   legal-d1234))))))

;; Problem 52
;; It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

(ns project-euler-clj.problem-052
  (:require [project-euler-clj.common :as common]))




(defn all-permuted?
  [seqs]
  (every? identity (map #(= (count (set %)) (count %))
                        (partition (count seqs)
                                   (apply interleave seqs)))))

(all-permuted? [[1 2 3] [3 1 2] [2 3 1]])

(defn solve []
  (first (filter (fn [[_ sq]] (and (apply = (map sort sq))))
                 (map (fn [x] [x
                               (map (fn [n] (common/num-to-digit-seq n))
                                    (map (fn [k] (* k x))
                                         (range 2 7)))])
                      (filter odd? (drop 10 (range)))))))

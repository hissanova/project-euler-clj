;; Problem 38
;; Take the number 192 and multiply it by each of 1, 2, and 3:

;; 192 × 1 = 192
;; 192 × 2 = 384
;; 192 × 3 = 576
;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

(ns project-euler-clj.problem-038
  (:require [project-euler-clj.common :as common]))

(def possible-digit-nums [[4 5] [3 3 3] [2 2 2 3 3] [1 2 2 2 2] [1 1 1 2 2 2] [1 1 1 1 1 2 2] [1 1 1 1 1 1 1 2] [1 1 1 1 1 1 1 1 1]])



(defn solve
  []
  (last (sort-by (fn [x] (nth x 1))
                 (map (fn [y] (update y 1 common/digit-seq-to-num))
                      (filter #(and (not-any? #{0} (nth % 1))
                                    (= 9 (count (nth % 1)))
                                    (= 9 (count (set (nth % 1)))))
                              (apply concat
                                     (map #(map (fn [n] [[n %] (apply concat
                                                                      (map (fn [i] (common/num-to-digit-seq (* n (inc i))))
                                                                           (range (count %))))])
                                                (common/gen-n-digits-nums (first %)))
                                          possible-digit-nums)))))))

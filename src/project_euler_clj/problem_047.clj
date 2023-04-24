;; Problem 47
;; <p>The first two consecutive numbers to have two distinct prime factors are:</p>
;; <p class="margin_left">14 = 2 × 7<br />15 = 3 × 5</p>
;; <p>The first three consecutive numbers to have three distinct prime factors are:</p>
;; <p class="margin_left">644 = 2² × 7 × 23<br />645 = 3 × 5 × 43<br />646 = 2 × 17 × 19.</p>
;; <p>Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?</p>

(ns project-euler-clj.problem-047
  (:require [project-euler-clj.common :as common]))

(defn k-distinct-prime-factors?
  [n k]
  (= k (count (common/prime-factor n))))

(defn- add-to-candidates-or-delete-them-all
  [n k candidates]
  (if (k-distinct-prime-factors? n k)
    (conj candidates n)
    []))
(add-to-candidates-or-delete-them-all 15 2 [14])

(defn find-first-k-consec-l-prime-fac-nums
  [k l]
  (loop [num-seq (map #(+ % 10) (range))
         candidates []]
    (if (= k (count candidates))
      candidates
      (recur (rest num-seq)
             (add-to-candidates-or-delete-them-all (first num-seq)
                                                   l
                                                   candidates)))))

(defn solve
  []
  (find-first-k-consec-l-prime-fac-nums 4 4))


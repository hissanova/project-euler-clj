;; Problem 35
;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

;; How many circular primes are there below one million?

(ns project-euler-clj.problem-035
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn rotate-seq
  [seq]
  (cons (last seq) (take (dec (count seq)) seq)))

(rotate-seq [1 2 3])

(defn full-rotation
  [seq]
  (map (fn [n] (vec ((apply comp (repeat n rotate-seq)) seq)))
       (range (count seq))))

(full-rotation [1 2 3])

(defn contains-even-d-0-or-5?
  [seq]
  (not-any? (fn [n] (or (contains? #{0 5} n) (even? n))) seq))

(defn get-candidates-upto
  [limit]
  (set (concat '((2) (5)) (filter contains-even-d-0-or-5?
                                               (map common/num-to-digit-seq
                                                    (common/get-primes-upto limit))))))

(defn solve [limit]
  (let [candidates (get-candidates-upto limit)]
    (count (sort-by common/digit-seq-to-num
                    (filter (fn [n] (every? #(contains? candidates %)
                                            (full-rotation n)))
                            candidates)))))

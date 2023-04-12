;; Problem 28
;; Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13

;; It can be verified that the sum of the numbers on the diagonals is 101.

;; What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

(ns project-euler-clj.problem-028
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn solve [n]
  (let [seq (conj (vec (flatten (map #(range (int (Math/pow (- %1 2) 2))
                                         (int (Math/pow %1 2))
                                         (dec %1))
                                 (range 3 (inc (* 2 (inc n))) 2))))
                  (* (inc (* 2 n)) (inc (* 2 n))))]
    [(reduce + seq) seq]))

;; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


;; How many such routes are there through a 20×20 grid?

(ns project-euler-clj.problem-15
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(defn combination
  [n r]
  (if (or (< n 0) (< r 0) (< n r))
    (throw (Exception. (format "Invalid arguments n=%d, r=%d" n r))))
  (bigint (/ (common/factorial n)
             (* (common/factorial (- n r)) (common/factorial r)))))

(combination 40N 20)

;; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.


;; How many such routes are there through a 20×20 grid?

(ns project-euler-clj.problem-15
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn factorial
  ([n]
   (if (< n 0)
     (throw (Exception. (format "Invalid argument %d" n))))
   (factorial n 1))
  ([n prod]
   (if (or (= n 0) (= n 1))
     prod
     (recur (dec n) (* n prod)))))

(defn combination
  [n r]
  (if (or (< n 0) (< r 0) (< n r))
    (throw (Exception. (format "Invalid arguments n=%d, r=%d" n r))))
  (bigint (/ (factorial n)
             (* (factorial (- n r)) (factorial r)))))

(combination 40N 20)
